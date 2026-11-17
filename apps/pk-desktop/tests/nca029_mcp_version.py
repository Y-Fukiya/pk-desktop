"""Check the shipped MCP version against the real R app version without a listener."""
import importlib.util
import io
import json
from pathlib import Path
import subprocess
import tempfile
from unittest.mock import patch

app = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("pkdesktop_mcp_test", app / "mcp_server.py")
m = importlib.util.module_from_spec(spec)
spec.loader.exec_module(m)
rversion = subprocess.check_output(["Rscript", "--vanilla", "-e", f"source({json.dumps(str(app / 'pkapp.R'))}); cat(environment(pkapp_open)$desktop_version)"], text=True).strip()
assert m.APP_VERSION == rversion, (m.APP_VERSION, rversion)
class Opener:
    def __init__(self, health): self.health, self.requests = health, []
    def open(self, request, timeout):
        self.requests.append(request)
        return io.StringIO(json.dumps(self.health if request.full_url.endswith("/health") else {"ok": True}))
with tempfile.TemporaryDirectory(prefix="nca029-") as folder:
    project = Path(folder).resolve()
    runtime = project / "runtime.json"
    info = {"api_port": 3880, "token": "a"*64, "instance_id": "test", "pid": 42, "mode": "nca", "project": str(project)}
    runtime.write_text(json.dumps(info)); runtime.chmod(0o600)
    health = {**info, "app_version": rversion, "contract_version": m.CONTRACT_VERSION}
    opener = Opener(health)
    with patch.object(m.urllib.request, "build_opener", return_value=opener):
        result = m.EngineClient(runtime, "nca").call("status")
    assert result["ok"] and len(opener.requests) == 2
    opener = Opener({**health, "app_version": "0.0.0-stale"})
    with patch.object(m.urllib.request, "build_opener", return_value=opener):
        result = m.EngineClient(runtime, "nca").call("status")
    assert result["error"]["code"] == "E_VERSION" and len(opener.requests) == 1
print("NCA029: R app and MCP versions match; current and stale health responses checked")
