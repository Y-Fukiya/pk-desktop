"""Exercise named-server updates without changing the user's Codex settings."""
import copy
import importlib.util
import json
from pathlib import Path
import subprocess
import tempfile
import unittest
from unittest.mock import patch

path = Path(__file__).resolve().parents[1] / "mcp_register.py"
spec = importlib.util.spec_from_file_location("registration", path)
registration = importlib.util.module_from_spec(spec)
spec.loader.exec_module(registration)


class RegistrationTests(unittest.TestCase):
    def setUp(self):
        self.folder = tempfile.TemporaryDirectory()
        self.project = Path(self.folder.name)
        self.old = {"command": "/old/python", "args": ["/old/release/mcp_server.py", "--mode", "nca", "--runtime", str(self.project / ".pkapp-runtime.json")]}
        self.new = {"command": "/new/python", "args": ["/new/release/mcp_server.py", "--mode", "nca", "--runtime", str(self.project / ".pkapp-runtime.json")]}
        self.other = {"name": "unrelated", "transport": {"type": "stdio", "command": "/other/private-command", "env": {"PRIVATE": "do-not-change"}}}
        self.entries = [copy.deepcopy(self.other)]
        self.calls = []
        self.fail_new = False

    def tearDown(self):
        self.folder.cleanup()

    def add_existing(self):
        self.entries.append({"name": "pkapp_nca", "enabled": True, "transport": {"type": "stdio", **copy.deepcopy(self.old)}})

    def cli(self, codex, *args):
        self.calls.append(args)
        if args == ("list", "--json"):
            return subprocess.CompletedProcess([], 0, json.dumps(self.entries), "")
        self.assertEqual(args[1], "pkapp_nca")
        if args == ("get", "pkapp_nca", "--json"):
            return subprocess.CompletedProcess([], 0, json.dumps(next(item for item in self.entries if item["name"] == "pkapp_nca")), "")
        if args[0] == "remove":
            self.entries = [entry for entry in self.entries if entry["name"] != args[1]]
        elif args[0] == "add":
            if self.fail_new and args[3] == self.new["command"]:
                raise RuntimeError("simulated add error")
            self.entries.append({"name": args[1], "transport": {"type": "stdio", "command": args[3], "args": list(args[4:])}})
        else:
            self.fail("Unexpected CLI operation")
        return subprocess.CompletedProcess([], 0, "", "")

    def run_registration(self, update=False):
        with patch.object(registration, "cli", self.cli):
            return registration.register("mock-codex", self.new, self.project, update)

    def test_new_registration_preserves_other_servers(self):
        self.assertEqual(self.run_registration(), "registered")
        self.assertEqual(self.entries[0], self.other)
        self.assertEqual(len(self.entries), 2)

    def test_update_requires_explicit_flag(self):
        self.add_existing()
        before = copy.deepcopy(self.entries)
        with self.assertRaisesRegex(ValueError, "--update"):
            self.run_registration()
        self.assertEqual(self.entries, before)

    def test_explicit_update_preserves_other_server(self):
        self.add_existing()
        self.assertEqual(self.run_registration(update=True), "updated")
        self.assertEqual(self.entries[0], self.other)
        self.assertEqual(self.entries[1]["transport"]["args"], self.new["args"])

    def test_same_registration_is_noop(self):
        self.old = copy.deepcopy(self.new)
        self.add_existing()
        self.assertEqual(self.run_registration(), "already_current")
        self.assertEqual(self.calls, [("list", "--json"), ("get", "pkapp_nca", "--json")])

    def test_different_project_is_not_overwritten(self):
        self.old["args"][-1] = "/other/project/.pkapp-runtime.json"
        self.add_existing()
        with self.assertRaisesRegex(ValueError, "No changes"):
            self.run_registration(update=True)
        self.assertEqual(self.calls, [("list", "--json"), ("get", "pkapp_nca", "--json")])

    def test_custom_settings_are_not_lost(self):
        for key, value in (("env", {"SECRET": "value"}), ("cwd", "/custom"), ("env_vars", ["KEY"])):
            with self.subTest(key=key):
                self.entries = [copy.deepcopy(self.other)]
                self.add_existing()
                self.entries[-1]["transport"][key] = value
                before = copy.deepcopy(self.entries)
                with self.assertRaises(ValueError):
                    self.run_registration(update=True)
                self.assertEqual(self.entries, before)

    def test_tool_restrictions_are_not_lost(self):
        for key in ("enabled_tools", "disabled_tools"):
            with self.subTest(key=key):
                self.entries = [copy.deepcopy(self.other)]
                self.add_existing()
                self.entries[-1][key] = ["pk_get_status"]
                before = copy.deepcopy(self.entries)
                with self.assertRaises(ValueError):
                    self.run_registration(update=True)
                self.assertEqual(self.entries, before)

    def test_failed_update_restores_previous_target(self):
        self.add_existing()
        self.fail_new = True
        with self.assertRaisesRegex(RuntimeError, "restored"):
            self.run_registration(update=True)
        self.assertEqual(self.entries[0], self.other)
        self.assertEqual(self.entries[1]["transport"], {"type": "stdio", **self.old})


if __name__ == "__main__":
    unittest.main()
