#!/usr/bin/env python3
"""Run the shipped NCA checks without installing packages or starting a server."""
from __future__ import annotations

import argparse
import datetime
import hashlib
import json
import os
from pathlib import Path
import platform
import shutil
import signal
import subprocess
import sys
import time

SOURCE = Path(__file__).resolve().parents[3]
DESKTOP = SOURCE / "apps/pk-desktop"
R_TESTS = ("pk_preprocessing", "nca", "nca_scientific", "nca_review", "nca_ui", "grid", "workbench", "preparation_ui",
           "nca011_core", "nca011_preparation", "nca011_ui", "nca012_core", "nca012_preparation", "nca012_ui",
           "nca013_serialization", "nca013_core", "nca013_flow", "nca013_ui", "nca014_diagnostics", "nca014_ui",
           "nca017_mcp", "nca017_pipeline", "nca017_ui", "nca018_contract", "nca018_owner", "nca018_ui", "nca019_import", "nca019_assembly", "nca019_context", "nca019_terminal", "nca019_contract", "nca019_ui", "nca020_annotation", "nca020_ui", "nca020_owner")


def checks(rscript: str) -> list[tuple[str, list[str]]]:
    r = [rscript, "--vanilla"]
    result = [(name, r + [str(DESKTOP / "tests" / f"{name}.R")]) for name in (*R_TESTS, "nca021_core", "nca021_annotation", "nca021_ui", "nca022_core", "nca022_ui", "nca023_ui", "nca024_core", "nca024_ui", "nca024_owner", "nca025_core", "nca025_ui", "nca026_core", "nca026_ui", "nca028_warnings", "nca029_diagnostics", "nca029_scroll_ui", "nca030_explore", "nca031_review", "nca032_review", "nca033_review", "nca034_review", "nca035_review", "nca036_review", "nca037_review", "nca038_review", "nca039_review", "nca040_review", "nca041_review", "nca042_review", "nca043_review", "nca044_review", "nca045_adpp", "nca046_adpp_review", "nca047_adpp_roles", "nca048_adpp_specimen", "nca049_adpp_mapping", "nca050_adpp_mapping", "nca051_adpp_suffix", "nca052_mapping_switch", "nca053_review", "nca054_review", "nca055_review")]
    result.append(("nca056_i18n", r + [str(DESKTOP / "tests/nca056_i18n.R")]))
    result.append(("nca057_review", r + [str(DESKTOP / "tests/nca057_review.R")]))
    result.append(("nca058_review", r + [str(DESKTOP / "tests/nca058_review.R")]))
    result.append(("nca059_review", r + [str(DESKTOP / "tests/nca059_review.R")]))
    result.append(("nca060_review", r + [str(DESKTOP / "tests/nca060_review.R")]))
    result.append(("nca061_first_use", r + [str(DESKTOP / "tests/nca061_first_use.R")]))
    result.extend([
        ("nca042_kill_restart", [sys.executable, str(DESKTOP / "tests/nca042_kill_restart.py")]),
        ("nca028_scroll", [shutil.which("node") or "node", str(DESKTOP / "tests/nca028_scroll.js")]),
        ("nca051_upload_i18n", [shutil.which("node") or "node", str(DESKTOP / "tests/nca051_upload_i18n.js")]),
        ("nca055_browser_args", [shutil.which("node") or "node", str(DESKTOP / "tests/nca055_browser_args_test.cjs")]),
        ("nca029_mcp_version", [sys.executable, str(DESKTOP / "tests/nca029_mcp_version.py")]),
        ("nca024_contract", [sys.executable, str(DESKTOP / "tests/nca024_contract.py")]),
        ("nca048_contract", [sys.executable, str(DESKTOP / "tests/nca048_contract.py")]),
        ("oral_absorption_reference", r + [str(DESKTOP / "examples/nca_dose_body/oral_reproduce.R")]),
        ("dose_body_reference", r + [str(DESKTOP / "examples/nca_dose_body/reproduce.R")]),
        ("nca022_cli", [sys.executable, str(DESKTOP / "tests/nca022_cli.py")]),
        ("nca_mcp_stdio_requires_mcp_python", [sys.executable, str(DESKTOP / "tests/nca017_mcp_contract.py")]),
        ("nca_mcp_config", [sys.executable, str(DESKTOP / "tests/nca017_config.py")]),
        ("nca_mcp_runtime_guards", [sys.executable, str(DESKTOP / "tests/nca017_runtime.py")]),
        ("nca018_mcp_contract", [sys.executable, str(DESKTOP / "tests/nca018_contract.py")]),
        ("nca020_mcp_contract", [sys.executable, str(DESKTOP / "tests/nca020_contract.py")]),
        ("nca021_mcp_contract", [sys.executable, str(DESKTOP / "tests/nca021_contract.py")]),
        ("intervening_dose_reference", r + [str(DESKTOP / "examples/nca_intervening_dose/reproduce.R")]),
        ("crossover_reference", r + [str(DESKTOP / "examples/nca_crossover/reproduce.R")]),
        ("adpp_hand_reference", r + [str(DESKTOP / "examples/adpp_hand_reference/compare.R")]),
        ("adpp_independent_python", [sys.executable, str(DESKTOP / "examples/adpp_independent_pk/reference.py"), "--check", str(DESKTOP / "examples/adpp_independent_pk/expected.csv")]),
        ("adpp_independent_compare", r + [str(DESKTOP / "examples/adpp_independent_pk/compare.R")]),
        ("adpp_auto_terminal_python", [sys.executable, str(DESKTOP / "examples/adpp_auto_terminal/reference.py"), "--check", str(DESKTOP / "examples/adpp_auto_terminal/expected.csv")]),
        ("adpp_auto_terminal_compare", r + [str(DESKTOP / "examples/adpp_auto_terminal/compare.R")]),
        ("nca019_mcp_contract", [sys.executable, str(DESKTOP / "tests/nca019_contract.py")]),
        ("nca018_registration", [sys.executable, str(DESKTOP / "tests/nca018_registration.py")]),
        ("browser_helper_models", [sys.executable, str(DESKTOP / "release/tests/test_nca014_browser_helpers.py")]),
        ("csv_acceptance", r + [str(DESKTOP / "tests/csv_parser.R")]),
        ("theoph_reference", r + [str(DESKTOP / "examples/theoph_reference/check_public_reference.R"), str(SOURCE)]),
        ("positional_blq_reference", r + [str(DESKTOP / "examples/positional_blq_reference.R"), str(SOURCE)]),
        ("saved_nca_pipeline", r + [str(DESKTOP / "release/tests/nca_pipeline.R")]),
        ("independent_nca_scenarios", r + [str(DESKTOP / "release/tests/nca_independent_scenarios.R")]),
        ("review_012_scenarios", r + [str(DESKTOP / "release/tests/nca012_review_scenarios.R")]),
        ("review_013_scenarios", r + [str(DESKTOP / "release/tests/nca013_review_scenarios.R")]),
        ("runtime_setup", r + [str(DESKTOP / "release/tests/runtime_setup.R")]),
        ("utf8_locale", [sys.executable, str(DESKTOP / "release/tests/utf8_locale.py")]),
        ("verification_runner", [sys.executable, str(DESKTOP / "release/tests/verify_release.py")]),
        ("source_package", [sys.executable, str(DESKTOP / "release/tests/package_source.py")]),
    ])
    return result


def run_check(name: str, command: list[str], output: Path, timeout: int) -> dict:
    log_path = output / f"{name}.log"
    wrapped = ["/bin/bash", "-c", '. "$1"; desktop_ensure_utf8_locale || exit 1; shift; exec "$@"',
               "pk-verify", str(DESKTOP / "utf8_locale.sh"), *command]
    started = time.monotonic()
    status = "failed"
    environment = dict(os.environ, R_USER_CACHE_DIR=str(output / "r-cache"))
    with log_path.open("wb") as log:
        process = subprocess.Popen(wrapped, cwd=SOURCE, env=environment, stdout=log, stderr=subprocess.STDOUT, start_new_session=True)
        try:
            code = process.wait(timeout=timeout)
            status = "passed" if code == 0 else "failed"
        except subprocess.TimeoutExpired:
            os.killpg(process.pid, signal.SIGKILL)
            code = process.wait()
            status = "timeout"
    contents = log_path.read_bytes()
    return {"id": name, "status": status, "exit_code": code, "seconds": round(time.monotonic() - started, 3),
            "command": command, "log": log_path.name, "log_sha256": hashlib.sha256(contents).hexdigest()}


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--rscript", default=os.environ.get("PK_RSCRIPT", "Rscript"))
    parser.add_argument("--output-dir", required=True, type=Path, help="New directory for logs and verification.json")
    parser.add_argument("--timeout", type=int, default=180, help="Maximum seconds per check")
    parser.add_argument("--with-browser", action="store_true", help="Also run the Playwright synthetic Shiny fixture (requires Playwright and Chromium)")
    args = parser.parse_args()
    rscript = shutil.which(args.rscript)
    if not rscript:
        parser.error("Rscript is unavailable; use --rscript /absolute/path/to/Rscript")
    if args.timeout <= 0:
        parser.error("--timeout must be positive")
    output = args.output_dir.resolve()
    if output.exists():
        parser.error("--output-dir must be a new directory; existing results are never overwritten")
    output.mkdir(parents=True)
    version = json.loads((DESKTOP / "src-tauri/tauri.conf.json").read_text())["version"]
    report = {"application_version": version, "source": str(SOURCE), "python": platform.python_version(),
              "platform": platform.platform(), "started_at": datetime.datetime.now(datetime.timezone.utc).isoformat(),
              "scope": "Shipped NCA/preprocessing checks, public reference, CSV acceptance, save/replay and runtime/package boundaries",
              "not_tested": ["authenticated full desktop browser workflows", "native binary", "signing/notarization", "clean network restore",
                             "repository-wide 37-drug harness", "full teal module integration", "live NCA MCP loopback (run tests/live_nca_mcp.py separately)"], "checks": []}
    plan = checks(rscript)
    if args.with_browser:
        plan.append(("nca054_browser", [shutil.which("node") or "node", str(DESKTOP / "tests/nca054_browser.cjs")]))
        plan.append(("nca057_browser", [shutil.which("node") or "node", str(DESKTOP / "tests/nca057_browser.cjs")]))
        plan.append(("nca058_browser", [shutil.which("node") or "node", str(DESKTOP / "tests/nca058_browser.cjs")]))
        plan.append(("nca059_browser", [shutil.which("node") or "node", str(DESKTOP / "tests/nca059_browser.cjs")]))
        plan.append(("nca060_browser", [shutil.which("node") or "node", str(DESKTOP / "tests/nca060_browser.cjs")]))
    report["synthetic_playwright_requested"] = args.with_browser
    for name, command in plan:
        result = run_check(name, command, output, args.timeout)
        report["checks"].append(result)
        report["status"] = "running"
        (output / "verification.json").write_text(json.dumps(report, indent=2, ensure_ascii=False) + "\n")
        print(f'{name}: {result["status"]} ({result["seconds"]:.3f} s)', flush=True)
    report["status"] = "passed" if all(x["status"] == "passed" for x in report["checks"]) else "failed"
    (output / "verification.json").write_text(json.dumps(report, indent=2, ensure_ascii=False) + "\n")
    print(f'Result: {report["status"]}; {output / "verification.json"}')
    return 0 if report["status"] == "passed" else 1


if __name__ == "__main__":
    sys.exit(main())
