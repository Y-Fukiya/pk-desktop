"""Offline helper and failure-receipt tests. These do not run a browser."""
import contextlib
import importlib.util
import io
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import types
import unittest
from unittest.mock import MagicMock, patch

HERE = Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location("nca_browser", HERE / "nca013_browser_e2e.py")
helper = importlib.util.module_from_spec(spec)
spec.loader.exec_module(helper)


class Handle:
    def __init__(self, value=None, actions=None, name=None, click_error=False, on_click=None):
        self.value = value
        self.actions = actions
        self.name = name
        self.click_error = click_error
        self.disposed = False
        self.on_click = on_click

    def json_value(self):
        return self.value

    def as_element(self):
        return self

    def click(self):
        self.actions.append((self.name, "click"))
        if self.click_error:
            raise RuntimeError("option click blocked")
        if self.on_click:
            self.on_click()

    def fill(self, label):
        self.actions.append((self.name, "fill", label))

    def focus(self):
        self.actions.append((self.name, "focus"))

    def evaluate(self, expression):
        if "dispose()" in expression:
            self.disposed = True
            return None
        return self.value

    def dispose(self):
        self.disposed = True


class PageModel:
    def __init__(self, selected="old", click_error=False, event_error=False):
        self.actions = []
        self.selected = selected
        self.click_error = click_error
        self.event_error = event_error
        self.inputs = 0
        self.input_hidden = bool(selected)
        self.events = Handle({"change": 1, "shiny": 0 if event_error else 1})

    def wait_for_function(self, expression, arg):
        if "events" in arg:
            self.actions.append(("wait", "value_and_events"))
            if self.event_error:
                raise TimeoutError("Shiny input event missing")
            return Handle(True)
        operation = arg["operation"]
        if operation == "state":
            return Handle({"ready": True, "selected": self.selected, "label": "old label"})
        if operation == "control":
            return Handle(actions=self.actions, name="control",
                          on_click=lambda: setattr(self, "input_hidden", False))
        if operation in ("input", "exposedInput"):
            if operation == "exposedInput" and self.input_hidden:
                raise TimeoutError("selected input remains hidden")
            self.inputs += 1
            return Handle(actions=self.actions, name="input" + str(self.inputs))
        if operation == "option":
            return Handle(actions=self.actions, name="option", click_error=self.click_error)
        if operation == "labelValues":
            return Handle(["saved-id"])
        raise AssertionError(operation)

    def evaluate_handle(self, expression, arg):
        self.actions.append(("observe", "events"))
        return self.events

    def evaluate(self, expression, arg):
        return {"ready": True, "selected": self.selected, "label": "replacement label"}


class BrowserHelperTests(unittest.TestCase):
    @unittest.skipUnless(shutil.which("node"), "Node is required for the offline Selectize model")
    def test_selected_only_options_and_instance_queries(self):
        result = subprocess.run(
            [shutil.which("node"), str(HERE / "nca014_selectize_model.js")],
            input=json.dumps({"query": helper.SELECTIZE_QUERY, "events": helper.SELECTIZE_EVENTS, "prepareTab": helper.PREPARE_TAB_QUERY}),
            text=True, capture_output=True, check=False)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_click_uses_reacquired_handles_and_current_label_then_events(self):
        page = PageModel()
        receipt = helper.choose(page, "field", "next")
        self.assertEqual(page.actions, [("observe", "events"), ("control", "click"),
                         ("input1", "fill", "replacement label"), ("option", "click"),
                         ("wait", "value_and_events")])
        self.assertFalse(page.input_hidden)
        self.assertEqual(receipt["interaction"], "clicked")
        self.assertEqual(receipt["shiny"], 1)
        self.assertTrue(page.events.disposed)

    def test_identical_value_does_not_wait_for_impossible_change_event(self):
        page = PageModel(selected="next")
        self.assertEqual(helper.choose(page, "field", "next")["interaction"], "already_selected")
        self.assertEqual(page.actions, [])

    def test_failed_physical_click_is_not_a_success_or_fallback(self):
        page = PageModel(click_error=True)
        with self.assertRaisesRegex(AssertionError, "field.*next.*physical selection failed"):
            helper.choose(page, "field", "next")
        self.assertNotIn(("wait", "value_and_events"), page.actions)
        self.assertTrue(page.events.disposed)

    def test_missing_shiny_event_cannot_pass_selection(self):
        page = PageModel(event_error=True)
        with self.assertRaisesRegex(AssertionError, "Shiny input event missing"):
            helper.choose(page, "field", "next")
        self.assertTrue(page.events.disposed)

    def test_saved_table_and_focus_share_instance_queries(self):
        page = PageModel()
        self.assertEqual(helper.value_for_label(page, "source", "Saved table"), "saved-id")
        helper.focus_selectize(page, "qc-policy")
        self.assertEqual(page.actions, [("input1", "focus")])

    def test_unready_value_failure_names_identifier_value_and_state(self):
        page = PageModel()
        with patch.object(page, "wait_for_function", side_effect=TimeoutError("not ready")):
            with self.assertRaisesRegex(AssertionError, "field.*unknown.*waiting for state"):
                helper.wait_selectize(page, "field", "unknown")

    def receipt(self, url, metadata=None):
        with tempfile.TemporaryDirectory() as temporary:
            output = Path(temporary) / "receipt.json"
            argv = [str(HERE / "nca013_browser_e2e.py"), "--output", str(output)]
            if metadata is not None:
                sidecar = Path(temporary) / "runtime.json"
                sidecar.write_text(json.dumps(metadata))
                argv += ["--runtime-metadata", str(sidecar)]
            with patch.object(sys, "argv", argv), patch.dict(os.environ, {"PK_DESKTOP_E2E_URL": url}), \
                    patch.dict(sys.modules, {"playwright": None, "playwright.sync_api": None}), \
                    contextlib.redirect_stdout(io.StringIO()):
                result = helper.main()
            self.assertEqual(result, 1)
            return json.loads(output.read_text())

    def test_default_browser_launch_is_preserved(self):
        fake = types.ModuleType("playwright.sync_api")
        fake.sync_playwright = MagicMock()
        fake.expect = MagicMock()
        launch = fake.sync_playwright.return_value.__enter__.return_value.chromium.launch
        launch.side_effect = RuntimeError("browser unavailable")
        with tempfile.TemporaryDirectory() as temporary:
            output = Path(temporary) / "receipt.json"
            with patch.object(sys, "argv", ["test", "--output", str(output)]), \
                    patch.dict(os.environ, {"PK_DESKTOP_E2E_URL": "http://localhost:3879"}), \
                    patch.dict(sys.modules, {"playwright": types.ModuleType("playwright"), "playwright.sync_api": fake}), \
                    patch.object(helper, "version", return_value="test-playwright"), \
                    contextlib.redirect_stdout(io.StringIO()):
                self.assertEqual(helper.main(), 1)
            launch.assert_called_once_with(headless=True)
            receipt = json.loads(output.read_text())
            self.assertEqual(receipt["status"], "not_run")
            self.assertEqual(receipt["step"], "launch_browser")

    def test_normalized_error_url_and_bare_token_are_redacted(self):
        fake = types.ModuleType("playwright.sync_api")
        fake.sync_playwright = MagicMock()
        fake.expect = MagicMock()
        browser = fake.sync_playwright.return_value.__enter__.return_value.chromium.launch.return_value
        browser.version = "test-browser"
        browser.new_page.return_value.goto.side_effect = RuntimeError(
            "net::ERR_CONNECTION_REFUSED at http://localhost:3879/?token=offline-synthetic-token "
            "(token offline-synthetic-token)")
        with tempfile.TemporaryDirectory() as temporary:
            output = Path(temporary) / "receipt.json"
            with patch.object(sys, "argv", ["test", "--output", str(output), "--browser-executable", "/example/Chromium"]), \
                    patch.dict(os.environ, {"PK_DESKTOP_E2E_URL": "http://localhost:3879?token=offline-synthetic-token"}), \
                    patch.dict(sys.modules, {"playwright": types.ModuleType("playwright"), "playwright.sync_api": fake}), \
                    patch.object(helper, "version", return_value="test-playwright"), \
                    contextlib.redirect_stdout(io.StringIO()):
                result = helper.main()
            fake.sync_playwright.return_value.__enter__.return_value.chromium.launch.assert_called_once_with(
                headless=True, executable_path="/example/Chromium")
            text = output.read_text()
            receipt = json.loads(text)
            self.assertEqual(result, 1)
            self.assertEqual(receipt["status"], "failed")
            self.assertEqual(receipt["step"], "connect_project")
            self.assertNotIn("offline-synthetic-token", text)
            self.assertIn("<local project URL>", receipt["error"])

    def test_missing_playwright_writes_not_run_receipt(self):
        receipt = self.receipt("http://localhost:3879/?token=test-token")
        self.assertEqual(receipt["status"], "not_run")
        self.assertEqual(receipt["step"], "prerequisites")
        self.assertEqual(receipt["checks"], [])
        self.assertIn("ModuleNotFoundError", receipt["errors"][0]["message"])
        self.assertIsNone(receipt["runtime"]["R"])
        self.assertIsNone(receipt["runtime"]["shiny"])

    def test_absent_url_writes_readable_receipt(self):
        receipt = self.receipt("")
        self.assertIn("Set PK_DESKTOP_E2E_URL", receipt["errors"][0]["message"])
        self.assertNotIn("<local project URL>", receipt["error"])

    def test_runtime_versions_only_from_explicit_sidecar(self):
        receipt = self.receipt("http://localhost:3879", {"R": "supplied-r", "shiny": "supplied-shiny"})
        self.assertEqual(receipt["runtime"]["R"], "supplied-r")
        self.assertEqual(receipt["runtime"]["shiny"], "supplied-shiny")
        self.assertEqual(receipt["runtime_sources"]["R"], "supplied sidecar")

    def test_invalid_sidecar_writes_failure_receipt(self):
        receipt = self.receipt("http://localhost:3879", {"R": 4.6})
        self.assertEqual(receipt["status"], "not_run")
        self.assertIn("nonempty version strings", receipt["errors"][0]["message"])


if __name__ == "__main__":
    unittest.main()
