"""Exercise runtime identity guards without opening any listening socket."""
import importlib.util
import asyncio
import io
import json
from pathlib import Path
import tempfile
from unittest.mock import patch

spec = importlib.util.spec_from_file_location("nca_mcp", Path(__file__).resolve().parents[1] / "mcp_server.py")
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)

class Opener:
    def __init__(self, health):
        self.health = health
        self.requests = []

    def open(self, request, timeout):
        self.requests.append(request)
        body = self.health if request.full_url.endswith("/health") else {"ok": True, "result": {"mode": "nca"}}
        return io.StringIO(json.dumps(body))

with tempfile.TemporaryDirectory(prefix="nca-runtime-") as tmp:
    project = Path(tmp).resolve()
    runtime = project / ".pkapp-runtime.json"
    original = {"api_port": 3880, "token": "a" * 64, "instance_id": "instance-a", "pid": 12345,
                "mode": "nca", "project": str(project), "contract_version": module.CONTRACT_VERSION, "app_version": module.APP_VERSION}

    def call(info=None, health=None, permissions=0o600):
        runtime.write_text(json.dumps(info or original))
        runtime.chmod(permissions)
        opener = Opener(health or original)
        with patch.object(module.urllib.request, "build_opener", return_value=opener):
            answer = module.EngineClient(runtime, "nca").call("status")
        return answer, opener.requests

    answer, requests = call()
    assert answer["ok"] and len(requests) == 2
    assert all(r.full_url.startswith("http://127.0.0.1:3880/") for r in requests)
    assert requests[-1].get_header("X-pkapp-contract") == module.CONTRACT_VERSION
    answer, requests = call(health={**original, "contract_version": "pkapp-nca-1"})
    assert answer["error"]["code"] == "E_VERSION" and len(requests) == 1
    answer, requests = call(health={**original, "app_version": "0.17.0-alpha"})
    assert answer["error"]["code"] == "E_VERSION" and len(requests) == 1
    answer, requests = call(info={**original, "project": str(project.parent)})
    assert answer["error"]["code"] == "E_PROJECT_CONFLICT" and not requests
    answer, requests = call(health={**original, "project": str(project.parent)})
    assert answer["error"]["code"] == "E_PROJECT_CONFLICT" and len(requests) == 1
    answer, requests = call(health={**original, "mode": "summary"})
    assert answer["error"]["code"] == "E_MODE" and len(requests) == 1
    answer, requests = call(health={**original, "instance_id": "stale"})
    assert answer["error"]["code"] == "E_NOT_RUNNING" and len(requests) == 1
    answer, requests = call(permissions=0o644)
    assert answer["error"]["code"] == "E_AUTH" and not requests
    answer, requests = call(info={**original, "api_port": "3880"})
    assert answer["error"]["code"] == "E_NOT_RUNNING" and not requests
print("NCA runtime guards: matching owner, path, health project, mode, instance, permissions and port: PASS")

async def check_tool_arguments():
    context = dict(project_id="project", base_revision=2, actor="tester", reason="artificial fixture")
    table_context = {key: value for key, value in context.items() if key != "reason"}
    calls = {
        "pk_import_table": dict(table_context, path="/project/a.csv", name="input"),
        "pk_prepare_data": dict(table_context, source="table-a", steps=[], name="prepared"),
        "pk_join_data": dict(table_context, left="table-a", right="table-b", by=["ID"], columns=["X"], join="left", name="joined"),
        "pk_nca_configure": dict(context, source="table-a", specification={"c0_method": "zero"}),
        "pk_nca_run": context,
        "pk_nca_get_result": dict(result_id="nca-result", project_id="project"),
        "pk_nca_save_result": dict(context, result_id="nca-result", name="saved"),
    }
    observed = []
    def dispatch(self, operation, arguments=None):
        observed.append(json.loads(json.dumps(arguments or {})))
        return {"ok": True, "result": {"operation": operation}}
    with patch.object(module.EngineClient, "call", dispatch):
        server = module.create_nca_server(Path("unused-runtime.json"))
        for name, arguments in calls.items():
            await server.call_tool(name, arguments)
            assert observed[-1] == arguments
    assert len(observed) == len(calls)
asyncio.run(check_tool_arguments())
print("NCA 7 argument-bearing tools: exact JSON-serializable payloads: PASS")
