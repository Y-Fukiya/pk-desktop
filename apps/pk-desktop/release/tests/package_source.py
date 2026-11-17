"""Offline packaging checks; never modify the input source or installed runtimes."""
from pathlib import Path
import importlib.util
import json
import shutil
import tempfile
import unittest
import zipfile

HELPER = Path(__file__).resolve().parents[1] / "package_source.py"
spec = importlib.util.spec_from_file_location("packager", HELPER)
packager = importlib.util.module_from_spec(spec)
spec.loader.exec_module(packager)
SOURCE = HELPER.parents[3]
VERSION = json.loads((SOURCE / "apps/pk-desktop/src-tauri/tauri.conf.json").read_text())["version"]


class PackageTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.scratch = tempfile.TemporaryDirectory(prefix="pk-package-")
        cls.root = Path(cls.scratch.name)
        cls.source = cls.root / "fixture"
        for path in packager.selected_files(SOURCE):
            packager.copy_regular(path, cls.source / path.relative_to(SOURCE))

    @classmethod
    def tearDownClass(cls):
        cls.scratch.cleanup()

    def test_deterministic_archive_and_integrity(self):
        first = self.root / "first" / f"PKDesktop-{VERSION}"
        second = self.root / "second" / first.name
        one = packager.stage(self.source, first, VERSION, None, [])
        two = packager.stage(self.source, second, VERSION, None, [])
        self.assertEqual(one, two)
        self.assertEqual(packager.archive(first, self.root / "first.zip"), packager.archive(second, self.root / "second.zip"))
        with zipfile.ZipFile(self.root / "first.zip") as archive:
            names = archive.namelist()
            self.assertFalse(any("/drugs/" in p or "/.runtime/" in p or p.endswith("CHANGELOG.md") for p in names))
            for p in ("tools/pk_review.py", "tools/make_adnca.R", "apps/pk-review/reference.docx", "outputs/pk-review/bundles/reviewed-repeated/ADPP.csv"):
                self.assertIn(f"{first.name}/source/{p}", names)
            for entry in archive.infolist():
                if entry.filename.endswith(".command"):
                    self.assertEqual((entry.external_attr >> 16) & 0o777, 0o755)
        self.assertGreater(len(one["files"]), 80)

    def test_rejects_wrong_version_without_creating_stage(self):
        target = self.root / "wrong-version"
        with self.assertRaisesRegex(ValueError, "version metadata"):
            packager.stage(self.source, target, "99.0.0", None, [])
        self.assertFalse(target.exists())

    def test_rejects_stale_mcp_contract_version(self):
        contract = self.source / "apps/pk-desktop/nca_mcp_contract.json"
        original = contract.read_text()
        try:
            payload = json.loads(original)
            payload["app_version"] = "0.0.0-stale"
            contract.write_text(json.dumps(payload))
            target = self.root / "stale-contract"
            with self.assertRaisesRegex(ValueError, "version metadata"):
                packager.stage(self.source, target, VERSION, None, [])
            self.assertFalse(target.exists())
        finally:
            contract.write_text(original)

    def test_rejects_symlink_and_unsafe_include(self):
        link = self.source / "apps/pk-desktop/R/unexpected-link"
        link.symlink_to(self.source / "LICENSE")
        try:
            with self.assertRaisesRegex(ValueError, "symlink"):
                packager.selected_files(self.source)
        finally:
            link.unlink()
        with self.assertRaisesRegex(ValueError, "top-level"):
            packager.stage(self.source, self.root / "unsafe-include", VERSION, self.root, ["../outside.txt"])

    def test_refuses_overwrite(self):
        with self.assertRaisesRegex(ValueError, "already exists"):
            packager.stage(self.source, self.source, VERSION, None, [])


if __name__ == "__main__":
    unittest.main()
