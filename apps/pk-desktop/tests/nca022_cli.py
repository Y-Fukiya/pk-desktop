"""The live annotation check accepts the documented project option and legacy path."""
import importlib.util
from pathlib import Path
import tempfile
import unittest
spec = importlib.util.spec_from_file_location("live", Path(__file__).with_name("live_nca020_mcp.py"))
live = importlib.util.module_from_spec(spec)
spec.loader.exec_module(live)
class CliTests(unittest.TestCase):
    def test_project_forms(self):
        with tempfile.TemporaryDirectory() as folder:
            output = str(Path(folder) / "result.json")
            for arguments in (["--project", folder], [folder]):
                self.assertEqual(live.parse_args([*arguments, "--output", output]).project, Path(folder).resolve())
    def test_ambiguous_and_missing_project_are_rejected(self):
        with tempfile.TemporaryDirectory() as folder:
            for arguments in ([], [folder, "--project", folder]):
                with self.assertRaises(SystemExit): live.parse_args([*arguments, "--output", str(Path(folder) / "out.json")])
    def test_existing_evidence_is_not_overwritten(self):
        with tempfile.TemporaryDirectory() as folder:
            output = Path(folder) / "out.json"
            output.write_text("retained")
            with self.assertRaises(SystemExit): live.parse_args(["--project", folder, "--output", str(output)])
            self.assertEqual(output.read_text(), "retained")
if __name__ == "__main__": unittest.main()
