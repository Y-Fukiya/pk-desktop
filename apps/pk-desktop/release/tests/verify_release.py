"""The verifier must retain failing output and stop its own timed-out process."""
from pathlib import Path
import hashlib
import importlib.util
import sys
import tempfile
import unittest

HELPER = Path(__file__).resolve().parents[1] / "verify_release.py"
spec = importlib.util.spec_from_file_location("verifier", HELPER)
verifier = importlib.util.module_from_spec(spec)
spec.loader.exec_module(verifier)


class VerifierTests(unittest.TestCase):
    def test_current_review_checks_are_registered(self):
        checks = dict(verifier.checks("Rscript"))
        for name in ("nca028_warnings", "nca028_scroll", "nca029_diagnostics", "nca029_mcp_version", "nca029_scroll_ui", "nca030_explore", "nca031_review", "nca032_review", "nca033_review", "nca034_review"):
            self.assertIn(name, checks)

    def test_success_log_matches_checksum(self):
        with tempfile.TemporaryDirectory(prefix="pk-verifier-") as folder:
            output = Path(folder)
            result = verifier.run_check("success", [sys.executable, "-c", "import sys; print(sys.argv[1])", "name with spaces"], output, 10)
            self.assertEqual(result["status"], "passed")
            content = (output / result["log"]).read_bytes()
            self.assertEqual(content, b"name with spaces\n")
            self.assertEqual(result["log_sha256"], hashlib.sha256(content).hexdigest())

    def test_failure_log_is_retained(self):
        with tempfile.TemporaryDirectory(prefix="pk-verifier-") as folder:
            output = Path(folder)
            result = verifier.run_check("failure", [sys.executable, "-c", "import sys; print('expected failure'); sys.exit(7)"], output, 10)
            self.assertEqual(result["status"], "failed")
            self.assertEqual(result["exit_code"], 7)
            self.assertIn("expected failure", (output / result["log"]).read_text())

    def test_timeout_is_not_success(self):
        with tempfile.TemporaryDirectory(prefix="pk-verifier-") as folder:
            result = verifier.run_check("timeout", [sys.executable, "-c", "import time; time.sleep(5)"], Path(folder), 1)
            self.assertEqual(result["status"], "timeout")
            self.assertNotEqual(result["exit_code"], 0)


if __name__ == "__main__":
    unittest.main()
