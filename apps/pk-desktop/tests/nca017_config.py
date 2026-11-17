"""Check both client configuration formats and preservation of venv executable paths."""
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
try:
    import tomllib
except ModuleNotFoundError:
    import tomli as tomllib

cli = Path(__file__).resolve().parents[1] / "mcp_config.py"
with tempfile.TemporaryDirectory(prefix="nca config ") as tmp:
    folder = Path(tmp); project = folder / "project with spaces"; project.mkdir()
    venv = folder / "virtual env"; venv.mkdir(); python = venv / "python"; python.symlink_to(sys.executable)
    def config(fmt):
        return subprocess.check_output([sys.executable, str(cli), "--project", str(project), "--python", str(python), "--format", fmt], text=True)
    toml = config("codex"); js = config("json")
    a = tomllib.loads(toml)["mcp_servers"]["pkapp_nca"]; b = json.loads(js)["mcpServers"]["pkapp_nca"]
    assert a == b and a["command"] == str(python.parent.resolve() / python.name) and "--mode" in a["args"] and "nca" in a["args"]
    assert "token" not in toml.lower() and "token" not in js.lower()
    assert a["args"][-1] == str(project.resolve() / ".pkapp-runtime.json")
print("NCA config paths, symlink and TOML/JSON: PASS")
