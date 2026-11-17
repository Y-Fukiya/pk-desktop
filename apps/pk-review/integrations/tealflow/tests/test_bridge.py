"""Focused configuration and canonical-data handoff tests; no server is started."""
import importlib.util
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch

HERE = Path(__file__).resolve().parent.parent
spec = importlib.util.spec_from_file_location("tealflow_bridge", HERE / "bridge.py")
bridge = importlib.util.module_from_spec(spec)
spec.loader.exec_module(bridge)
sys.path.insert(0, str(bridge.ROOT))
from tools import pk_review


class ConfigurationTests(unittest.TestCase):
    def test_two_independent_interpreters_and_absolute_commands(self):
        pk = bridge.ROOT / ".venv/bin/python"
        tf = bridge.ROOT / ".runtime/tealflow/bin/python"
        servers = bridge.client_config(tf, pk)["mcpServers"]
        self.assertEqual(servers["pkdummy-review"]["command"], str(pk))
        self.assertEqual(servers["tealflow-mcp"]["command"], str(tf))
        self.assertNotEqual(servers["pkdummy-review"]["command"], servers["tealflow-mcp"]["command"])
        self.assertEqual(servers["tealflow-mcp"]["args"][-1], "serve")

    def test_runtime_must_match_supported_contract(self):
        for info in [dict(python=[3, 10, 0], tealflow_mcp="0.2.0", mcp="1.26.0"),
                     dict(python=[3, 12, 0], tealflow_mcp="0.3.0", mcp="1.26.0"),
                     dict(python=[3, 12, 0], tealflow_mcp="0.2.0", mcp="2.0.0")]:
            result = subprocess.CompletedProcess([], 0, json.dumps(info), "")
            with self.subTest(info=info), patch.object(bridge.subprocess, "run", return_value=result):
                with self.assertRaises(ValueError):
                    bridge.runtime_info(Path("/unused/python"))


class HandoffTests(unittest.TestCase):
    def test_saved_filters_and_input_values_survive_handoff(self):
        view = bridge.ROOT / "outputs/pk-review/views/round8-fixed-repeated.json"
        if not view.exists():
            self.skipTest("Prepared repeated-dose demo is not available")
        original = view.read_bytes()
        with tempfile.TemporaryDirectory(prefix="test-", dir=HERE) as tmp:
            tmp = Path(tmp)
            copied = tmp / "view.json"
            copied.write_bytes(original)
            target = tmp / "handoff"
            result = bridge.handoff(copied, target)
            self.assertFalse(result["raw_records_copied"])
            metadata = json.loads((target / "handoff.json").read_text())
            self.assertEqual([d["name"] for d in metadata["datasets"]], ["ADNCA", "ADPP"])
            self.assertNotIn("subjects", metadata)
            r = """source('data.R')
              pk <- load_pk_handoff()
              h <- jsonlite::fromJSON('handoff.json', simplifyVector=FALSE)
              source(file.path(h$root, 'apps/pk-review/R/data.R'))
              source(file.path(h$root, 'apps/pk-review/R/analysis.R'))
              expected <- pk_analyse(pk_load_bundle(h$bundle), pk_read_config(h$view))
              stopifnot(identical(pk$ADNCA, expected$concentrations),
                        identical(pk$ADPP, expected$parameters),
                        identical(pk$counts, expected$counts))
              cat(jsonlite::toJSON(pk$counts, auto_unbox=TRUE))
            """
            proc = subprocess.run(["Rscript", "-e", r], cwd=target,
                                  env=pk_review.r_environment(), capture_output=True, text=True, timeout=120)
            self.assertEqual(proc.returncode, 0, proc.stderr)
            self.assertEqual(json.loads(proc.stdout), metadata["counts"])
            with self.assertRaises(ValueError):
                bridge.handoff(copied, target)
            copied.write_bytes(original + b"\n")
            proc = subprocess.run(["Rscript", "-e", "source('data.R'); load_pk_handoff()"],
                                  cwd=target, env=pk_review.r_environment(), capture_output=True, text=True, timeout=120)
            self.assertNotEqual(proc.returncode, 0)
            self.assertIn("Saved PK settings changed", proc.stderr)
        self.assertEqual(view.read_bytes(), original)


if __name__ == "__main__":
    unittest.main()
