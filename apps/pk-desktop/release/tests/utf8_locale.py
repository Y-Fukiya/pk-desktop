"""Exercise the launcher's locale boundary without starting a server."""
from pathlib import Path
import os
import subprocess
import tempfile
import unittest

DESKTOP = Path(__file__).resolve().parents[2]


def shell(code, env=None):
    return subprocess.run(["/bin/bash", "-c", code, "test", str(DESKTOP)],
                          env=env, text=True, capture_output=True, timeout=20)


class LocaleTests(unittest.TestCase):
    def test_c_locale_becomes_available_utf8(self):
        env = dict(os.environ, LC_ALL="C", LANG="C")
        result = shell('. "$1/utf8_locale.sh"; desktop_ensure_utf8_locale && locale charmap', env)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout.strip().replace("-", "").upper(), "UTF8")

    def test_existing_utf8_settings_are_preserved(self):
        candidates = subprocess.check_output(["locale", "-a"], text=True).splitlines()
        chosen = next(x for x in candidates if "UTF-8" in x or "utf8" in x)
        env = dict(os.environ, LC_ALL=chosen, LANG="C", LC_TIME="C")
        result = shell('. "$1/utf8_locale.sh"; desktop_ensure_utf8_locale && printf "%s|%s|%s" "$LC_ALL" "$LANG" "$LC_TIME"', env)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, f"{chosen}|C|C")

    def test_missing_utf8_locale_fails_before_launch(self):
        result = shell('locale() { if [ "$1" = -a ]; then printf "C\\nPOSIX\\n"; else printf "US-ASCII\\n"; fi; }; . "$1/utf8_locale.sh"; desktop_ensure_utf8_locale')
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("requires an available UTF-8 locale", result.stderr)

    def test_available_but_unusable_locale_is_rejected(self):
        result = shell('locale() { if [ "$1" = -a ]; then printf "C.UTF-8\\n"; else printf "US-ASCII\\n"; fi; }; . "$1/utf8_locale.sh"; desktop_ensure_utf8_locale')
        self.assertNotEqual(result.returncode, 0)

    def test_launcher_passes_utf8_and_preserves_arguments(self):
        with tempfile.TemporaryDirectory(prefix="pk-locale-") as folder:
            fake = Path(folder) / "Rscript"
            fake.write_text('#!/bin/bash\nlocale charmap\nprintf "<%s>\\n" "$@"\n')
            fake.chmod(0o755)
            env = dict(os.environ, LC_ALL="C", LANG="C", PK_RSCRIPT=str(fake), PK_DESKTOP_NO_PAUSE="1")
            result = subprocess.run(["/bin/bash", str(DESKTOP / "launch_nca_macos.command"), "--project", "name with spaces"],
                                    env=env, text=True, capture_output=True, timeout=20)
            if os.uname().sysname != "Darwin":
                self.assertNotEqual(result.returncode, 0)
                return
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertIn("UTF-8", result.stdout)
            self.assertIn("<--mode>\n<nca>\n<--project>\n<name with spaces>", result.stdout)


if __name__ == "__main__":
    unittest.main()
