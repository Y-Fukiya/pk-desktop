#!/usr/bin/env python3
"""Exercise a running, disposable PK Desktop NCA project through browser controls."""
import argparse
import json
import os
import platform
import re
from importlib.metadata import version
from pathlib import Path
import time
from urllib.parse import parse_qsl, urlsplit


SELECTIZE_QUERY = r"""({identifier, value, label, operation = 'state'}) => {
    const select = document.getElementById(identifier);
    const instance = select && select.selectize;
    const control = instance && instance.$control && instance.$control[0];
    const input = instance && instance.$control_input && instance.$control_input[0];
    const dropdown = instance && instance.$dropdown_content && instance.$dropdown_content[0];
    const state = {ready: false, identifier, value, reason: null};
    if (!select || !select.isConnected) state.reason = 'original select missing';
    else if (!instance) state.reason = 'Selectize instance missing';
    else if (select.disabled || instance.isDisabled || instance.isLocked) state.reason = 'control disabled or locked';
    else if (!control || !control.isConnected || !input || !input.isConnected || input.disabled || !dropdown || !dropdown.isConnected)
        state.reason = 'current instance handles unavailable';
    else if (value !== undefined && !Object.prototype.hasOwnProperty.call(instance.options, value))
        state.reason = 'exact option key missing';
    else {
        state.ready = true;
        state.selected = select.value;
        if (value !== undefined) state.label = String(instance.options[value][instance.settings.labelField] ?? '');
    }
    if (operation === 'state') return state;
    if (!state.ready) return null;
    if (operation === 'control') return control;
    if (operation === 'input') return input;
    if (operation === 'exposedInput') {
        const rect = input.getBoundingClientRect();
        const style = getComputedStyle(input);
        return !instance.isInputHidden && rect.width > 0 && rect.height > 0 &&
            style.display !== 'none' && style.visibility !== 'hidden' && style.opacity !== '0' ? input : null;
    }
    if (operation === 'labelValues') return Object.keys(instance.options).filter(
        key => String(instance.options[key][instance.settings.labelField] ?? '') === label);
    if (operation === 'option') {
        const matches = Array.from(dropdown.querySelectorAll('[data-selectable][data-value]')).filter(node => {
            const rect = node.getBoundingClientRect();
            const style = getComputedStyle(node);
            return node.getAttribute('data-value') === value && rect.width > 0 && rect.height > 0 &&
                style.display !== 'none' && style.visibility !== 'hidden' &&
                node.getAttribute('aria-disabled') !== 'true';
        });
        return matches.length === 1 ? matches[0] : null;
    }
    throw new Error('Unknown Selectize query operation ' + operation);
}"""

PREPARE_TAB_QUERY = r"""() => {
    const tab = document.querySelector('#workbench-mode a[data-value="prepare"]');
    if (!tab) return false;
    const parent = tab.parentElement;
    const active = tab.getAttribute('aria-selected') === 'true' || tab.classList.contains('active') ||
        (parent && parent.tagName === 'LI' && parent.classList.contains('active'));
    const target = tab.getAttribute('aria-controls') || (tab.getAttribute('href') || '').replace(/^#/, '');
    const panel = target && document.getElementById(target);
    if (!active || !panel) return false;
    const rect = panel.getBoundingClientRect();
    const style = getComputedStyle(panel);
    return rect.width > 0 && rect.height > 0 && style.display !== 'none' && style.visibility !== 'hidden';
}"""

SELECTIZE_EVENTS = r"""({identifier, value}) => {
    if (!window.jQuery) throw new Error('jQuery event observation unavailable');
    const receipt = {change: 0, shiny: 0};
    const change = event => {
        if (event.target === document.getElementById(identifier) && event.target.value === value) receipt.change++;
    };
    const shiny = event => {
        if (event.name === identifier && event.value === value) receipt.shiny++;
    };
    window.jQuery(document).on('change', change).on('shiny:inputchanged', shiny);
    receipt.dispose = () => window.jQuery(document).off('change', change).off('shiny:inputchanged', shiny);
    return receipt;
}"""


def wait_selectize(page, identifier, value=None, operation="state", label=None):
    request = {"identifier": identifier, "operation": operation}
    if value is not None:
        request["value"] = value
    if label is not None:
        request["label"] = label
    predicate = "result.ready" if operation == "state" else "result && " + (
        "result.length === 1" if operation == "labelValues" else "true")
    try:
        return page.wait_for_function(
            "arg => { const result = (" + SELECTIZE_QUERY + ")(arg); return " + predicate + " ? result : false; }",
            arg=request)
    except Exception as error:
        state = page.evaluate(SELECTIZE_QUERY, {**request, "operation": "state"})
        raise AssertionError(f"Selectize {identifier!r}, value {value!r}, label {label!r}: "
                             f"waiting for {operation}; state={state}") from error


def choose(page, identifier, value):
    """Choose one exact value by clicking the current instance's visible option."""
    ready = wait_selectize(page, identifier, value)
    state = ready.json_value()
    ready.dispose()
    if state["selected"] == value:
        return {"identifier": identifier, "value": value, "interaction": "already_selected"}
    events = page.evaluate_handle(SELECTIZE_EVENTS, {"identifier": identifier, "value": value})
    try:
        control = wait_selectize(page, identifier, value, "control")
        try:
            control.as_element().click()
        finally:
            control.dispose()
        control = wait_selectize(page, identifier, value, "exposedInput")
        try:
            label = page.evaluate(SELECTIZE_QUERY, {"identifier": identifier, "value": value})["label"]
            control.as_element().fill(label)
        finally:
            control.dispose()
        option = wait_selectize(page, identifier, value, "option")
        try:
            option.as_element().click()
        finally:
            option.dispose()
        page.wait_for_function(
            "({identifier, value, events}) => { const select = document.getElementById(identifier); "
            "return select && select.value === value && events.change > 0 && events.shiny > 0; }",
            arg={"identifier": identifier, "value": value, "events": events})
        return {"identifier": identifier, "value": value, "interaction": "clicked",
                **events.evaluate("receipt => ({change: receipt.change, shiny: receipt.shiny})")}
    except Exception as error:
        state = page.evaluate(SELECTIZE_QUERY, {"identifier": identifier, "value": value})
        counts = events.evaluate("receipt => ({change: receipt.change, shiny: receipt.shiny})")
        raise AssertionError(f"Selectize {identifier!r}, value {value!r}: physical selection failed; "
                             f"state={state}, events={counts}; {error}") from error
    finally:
        events.evaluate("receipt => receipt.dispose()")
        events.dispose()


def value_for_label(page, identifier, label):
    handle = wait_selectize(page, identifier, operation="labelValues", label=label)
    try:
        return handle.json_value()[0]
    finally:
        handle.dispose()


def focus_selectize(page, identifier):
    control = wait_selectize(page, identifier, operation="input")
    try:
        control.as_element().focus()
    finally:
        control.dispose()


def redact_error(message, url):
    message = re.sub(r"https?://[^\s\"'<>]+", "<local project URL>", message)
    try:
        tokens = [value for key, value in parse_qsl(urlsplit(url).query) if key == "token" and value]
    except ValueError:
        tokens = []
    for token in tokens:
        message = message.replace(token, "<token>")
    return message


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("--headed", action="store_true")
    parser.add_argument("--browser-executable", type=Path, help="Optional Chromium-compatible browser executable")
    parser.add_argument("--runtime-metadata", type=Path, help='JSON sidecar with observed server version strings, e.g. {"R":"4.6.0","shiny":"1.13.0"}')
    args = parser.parse_args()
    if args.output.exists():
        parser.error("Choose a new output file")
    url = os.environ.get("PK_DESKTOP_E2E_URL", "")
    checks = []
    report = {"scope": "Live browser UI, actual form defaults, synthetic example only",
              "status": "not_run", "step": "prerequisites", "checks": checks, "errors": [],
              "runtime": {"python": platform.python_version(), "playwright": None,
                          "chromium": None, "R": None, "shiny": None},
              "runtime_sources": {"python": "test process"}}
    started = time.monotonic()
    try:
        if args.runtime_metadata:
            metadata = json.loads(args.runtime_metadata.read_text())
            if not isinstance(metadata, dict):
                raise ValueError("Runtime metadata must be a JSON object")
            for key in ("R", "shiny"):
                if key in metadata:
                    if not isinstance(metadata[key], str) or not metadata[key].strip():
                        raise ValueError("Runtime metadata values must be nonempty version strings")
                    report["runtime"][key] = metadata[key]
                    report["runtime_sources"][key] = "supplied sidecar"
        parsed = urlsplit(url)
        if parsed.scheme not in ("http", "https") or parsed.hostname not in ("127.0.0.1", "localhost", "::1"):
            raise ValueError("Set PK_DESKTOP_E2E_URL to the running local disposable project's URL")
        from playwright.sync_api import sync_playwright, expect
        report["runtime"]["playwright"] = version("playwright")
        report["runtime_sources"]["playwright"] = "test process"
        example = Path(__file__).resolve().parents[2] / "examples/nca_actual_time_example.csv"
        report["step"] = "launch_browser"
        with sync_playwright() as playwright:
            launch_options = {"headless": not args.headed}
            if args.browser_executable is not None:
                launch_options["executable_path"] = str(args.browser_executable)
            browser = playwright.chromium.launch(**launch_options)
            report["status"] = "running"
            report["runtime"]["chromium"] = browser.version
            report["runtime_sources"]["chromium"] = "launched browser"
            try:
                report["step"] = "connect_project"
                page = browser.new_page(viewport={"width": 1440, "height": 900})
                page.set_default_timeout(30000)
                page.goto(url, wait_until="domcontentloaded")
                page.wait_for_function("window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$socket && Shiny.shinyapp.$socket.readyState === WebSocket.OPEN")

                observed_shiny = page.evaluate("() => typeof Shiny.version === 'string' ? Shiny.version : null")
                if observed_shiny:
                    report["runtime"]["shiny"] = observed_shiny
                    report["runtime_sources"]["shiny"] = "observed page"
                report["selections"] = []

                def select_value(identifier, value):
                    report["selections"].append(choose(page, identifier, value))

                def rule(name, value):
                    radio = page.locator('input[name="workbench-' + name + '"][value="' + value + '"]')
                    radio.locator("xpath=ancestor::label[1]").click()
                    expect(radio).to_be_checked()

                report["step"] = "load_example"
                page.locator('#workspace_navigation a[data-value="data"]').click()
                page.locator("#workbench-file").set_input_files(str(example))
                expect(page.locator("#workbench-file_progress .progress-bar")).to_have_text(
                    re.compile(r"^(?:アップロード完了|Upload complete)$"))
                page.locator("#workbench-load_source").click()
                page.wait_for_function(PREPARE_TAB_QUERY)
                report["step"] = "elapsed_time"
                select_value("workbench-op", "elapsed_time")
                expect(page.locator("#workbench-sample_column")).to_have_value("SAMPLE_DTM")
                expect(page.locator("#workbench-dose_column")).to_have_value("DOSE_DTM")
                page.locator("#workbench-apply_step_body").click()
                expect(page.locator("#workbench-op")).to_have_value("")
                report["step"] = "blq_rules"
                select_value("workbench-op", "blq_rules")
                expect(page.locator("#workbench-blq_time")).to_have_value("NCA_TIME_H")
                expect(page.locator("#workbench-lloq_source")).to_have_value("column")
                expect(page.locator("#workbench-lloq_column")).to_have_value("LLOQ")
                checks.append("LLOQ source and column selected automatically; no LLOQ input changed")
                select_value("workbench-prep_flag_convention", "y_blank")
                select_value("workbench-zero_time", "pre")
                report["step"] = "blq_layout_and_keyboard"
                for width in (1440, 600):
                    page.set_viewport_size({"width": width, "height": 900})
                    markers = page.locator('.pk-blq-rule-row input[type="radio"]:visible')
                    expect(markers).to_have_count(20)
                    contained = markers.evaluate_all("nodes => nodes.every(n => { const r=n.getBoundingClientRect(), b=n.closest('label').getBoundingClientRect(); return r.width>0 && r.left>=b.left && r.right<=b.right && r.top>=b.top && r.bottom<=b.bottom; })")
                    if not contained:
                        raise AssertionError("BLQ radio marker outside its segment")
                    if width == 600:
                        compact = page.locator('.pk-blq-rule-row:has(input[type="radio"]):visible').evaluate_all("""rows => rows.every(row => {
                            const options = Array.from(row.querySelectorAll('label.radio-inline'), n => n.getBoundingClientRect());
                            const title = row.querySelector('.pk-blq-rule-label > strong').getBoundingClientRect();
                            const count = row.querySelector('.pk-blq-rule-count').getBoundingClientRect();
                            return options.length === 4 && options.every(r => Math.abs(r.top-options[0].top) < 1) &&
                                count.top >= title.bottom && count.right <= options[0].left;
                        })""")
                        if not compact:
                            raise AssertionError("600px BLQ options wrapped or count not below category")
                page.set_viewport_size({"width": 1440, "height": 900})
                pre_record = page.locator('input[name="workbench-pre_rule"][value="keep"]')
                pre_record.focus()
                page.keyboard.press("ArrowRight")
                pre_zero = page.locator('input[name="workbench-pre_rule"][value="zero"]')
                expect(pre_zero).to_be_checked()
                expect(pre_zero).to_be_focused()
                outline = pre_zero.locator("xpath=ancestor::label[1]").evaluate("n => { const s=getComputedStyle(n); return s.outlineStyle !== 'none' && parseFloat(s.outlineWidth)>0; }")
                if not outline:
                    raise AssertionError("Keyboard radio focus has no visible segment outline")
                checks.append("BLQ markers contained at both widths; 600px options on one row with count below category; arrow-key selection and focus outline")
                for name, value in (("pre_rule", "zero"), ("first_rule", "zero"),
                                    ("middle_rule", "exclude"), ("last_rule", "exclude"), ("all_blq_rule", "exclude")):
                    rule(name, value)
                page.locator("#workbench-apply_step_body").click()
                expect(page.locator("#workbench-op")).to_have_value("")
                report["step"] = "save_prepared_table"
                name = "【テスト用】NCA E2E " + str(time.time_ns())
                page.locator("#workbench-save_name").fill(name)
                page.locator("#workbench-save_result").click()
                expect(page.locator("#workbench-save_status")).to_contain_text("保存済み：" + name)
                report["saved_test_table"] = {"name": name, "retained": True}
                report["step"] = "nca_mapping"
                page.locator('#workspace_navigation a[data-value="nca"]').click()
                table_id = value_for_label(page, "nca-source", name)
                report["saved_test_table"]["id"] = table_id
                select_value("nca-source", table_id)
                expect(page.locator("#nca-time")).to_have_value("NCA_TIME_H")
                expect(page.locator("#nca-concentration")).to_have_value("NCA_CONC")
                for key, identifier, value in (("dosing", "nca-dosing_context", "ongoing"),
                                               ("c0", "nca-c0_method", "zero")):
                    page.locator(f"#{identifier}").select_option(value)
                    status = page.locator(f"#nca-review_{key} [data-review-state]")
                    expect(status).to_have_attribute("data-review-value", value)
                for key in ("dosing", "c0", "units"):
                    page.locator(f"#nca-confirm_{key}").click()
                    status = page.locator(f"#nca-review_{key} [data-review-state]")
                    expect(status).to_have_attribute("data-review-state", "confirmed")
                select_value("nca-auc_method", "linear")
                qc = page.locator("#nca-predose_qc")
                qc.focus()
                page.keyboard.press("Space")
                expect(qc).to_be_checked()
                page.locator("#nca-advanced-summary").click()
                page.locator("#nca-predose_qc_threshold_pct").fill("5")
                page.locator("#nca-predose_qc_window_h").fill("1")
                report["step"] = "nca_calculation"
                page.locator("#nca-run").click()
                table = page.locator("#nca-wide_table table")
                expect(table.locator("tbody tr")).to_have_count(1)
                cells = table.evaluate("table => { const h=Array.from(table.querySelectorAll('thead th'),n=>n.textContent.trim()); const v=Array.from(table.querySelectorAll('tbody tr:first-child td'),n=>n.textContent.trim()); return Object.fromEntries(h.map((key,i)=>[key,v[i]])); }")
                for parameter, expected in (("CMAX", 8), ("TMAX", 1), ("AUCLST", 31.5)):
                    values = [value for label, value in cells.items()
                              if label == parameter or label.startswith(parameter + " (")]
                    if len(values) != 1 or float(values[0]) != expected:
                        raise AssertionError(f"Unexpected {parameter}: {values!r}, expected {expected}")
                if cells.get("PROFILE_STATUS") != "計算済み":
                    raise AssertionError("Default workflow did not produce an OK profile")
                page.locator('#nca-result_view a').filter(has_text="解析単位の詳細").click()
                detail = page.locator("#nca-profile_details")
                expect(detail.locator(".pk-profile-qc-metadata .is-success")).to_have_text("BLQの上限が閾値以下")
                expect(detail).not_to_contain_text("レシピ")
                expect(detail).not_to_contain_text("missing_lloq")
                expect(detail).not_to_contain_text("方針による値：。")
                checks.append("Load / elapsed / BLQ / save / NCA: 8, 1, 31.5 and OK")
                report["step"] = "qc_focus_visibility"
                for width in (1440, 600):
                    page.set_viewport_size({"width": width, "height": 900})
                    focus_selectize(page, "nca-terminal_method")
                    page.evaluate("() => { const e=document.getElementById('nca-predose_qc'); window.scrollBy(0,e.getBoundingClientRect().top-865); }")
                    page.keyboard.press("Tab")
                    expect(qc).to_be_focused()
                    page.wait_for_function("() => { const e=document.getElementById('nca-predose_qc').getBoundingClientRect(); const b=document.querySelector('.pk-nca-runbar').getBoundingClientRect(); return e.top >= 0 && e.bottom < b.top; }")
                    checks.append("Tab-focused QC input above action bar at width " + str(width))
                report["step"] = "complete"
                report["status"] = "passed"
                report["result"] = cells
            finally:
                browser.close()
    except Exception as error:
        if report["status"] != "not_run":
            report["status"] = "failed"
        report["error"] = redact_error(type(error).__name__ + ": " + str(error), url)
        report["errors"].append({"step": report["step"], "message": report["error"]})
    report["seconds"] = round(time.monotonic() - started, 3)
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(report, ensure_ascii=False, indent=2) + "\n")
    print(report["status"] + ": " + str(args.output))
    return 0 if report["status"] == "passed" else 1


if __name__ == "__main__":
    raise SystemExit(main())
