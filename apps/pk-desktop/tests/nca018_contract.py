"""Exercise schema errors and initialized-client metadata over real stdio."""
import asyncio
import importlib.util
import json
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
import sys
import tempfile
from threading import Thread
from unittest.mock import patch
from types import SimpleNamespace

from mcp import ClientSession, StdioServerParameters, types
from mcp.client.stdio import stdio_client

script = Path(__file__).resolve().parents[1] / "mcp_server.py"
spec = importlib.util.spec_from_file_location("nca018_mcp", script)
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)
long_client = SimpleNamespace(name="N" * 240, version="V" * 140)
mock_context = SimpleNamespace(session=SimpleNamespace(client_params=SimpleNamespace(clientInfo=long_client)))
assert module.initialized_client(SimpleNamespace(get_context=lambda: mock_context))["name"] == "N" * 200
assert module.initialized_client(SimpleNamespace(get_context=lambda: mock_context))["version"] == "V" * 120
secret = "PRIVATE_INPUT_VALUE_919"

valid = [
    {"op": "filter", "column": "ID", "operator": "eq", "values": ["S03"]},
    {"op": "filter", "column": "ID", "operator": "missing"},
    {"op": "select", "columns": ["ID", "CONC"]},
    {"op": "rename", "column": "CONC", "name": "VALUE"},
    {"op": "convert", "column": "CONC", "type": "numeric"},
    {"op": "sort", "columns": ["ID"], "decreasing": True},
    {"op": "distinct", "columns": ["ID"]},
    {"op": "where", "predicate": {"op": "%in%", "args": [{"column": "ID"}, {"value": ["S03"]}]}},
    {"op": "elapsed_time", "sample_column": "SAMPLE", "dose_column": "DOSE", "sample_datetime_type": "sas_seconds"},
    {"op": "blq_rules", "mode": "positional", "profile_columns": ["ID"], "concentration": "CONC", "time": "TIME", "blq_column": "BLQ", "pre_rule": "zero", "first_rule": "zero", "middle_rule": "exclude", "last_rule": "exclude"},
]
invalid = [
    {"op": "filter", "column": "ID", "operator": "eq", "value": secret},
    {"op": "filter", "column": "ID", "operator": secret, "values": [secret]},
    {"op": "filter", "column": "ID", "operator": "eq", "values": secret},
    {"op": "filter", "column": "ID", "operator": "eq", "values": []},
    {"op": "filter", "column": "ID", "operator": "eq", "values": [1, 2]},
    {"op": "select", "columns": ["ID", "ID"]},
    {"op": "rename", "column": "ID", "new_name": secret},
    {"op": "convert", "column": "ID", "type": secret},
    {"op": "sort", "columns": ["ID"], "decreasing": secret},
    {"op": "distinct", "columns": secret},
    {"op": "where", "predicate": {"op": "system", "args": [{"value": secret}]}},
    {"op": "elapsed_time", "sample_column": "SAMPLE", "dose_column": "DOSE", "output": secret, "utc_offset": "bad"},
    {"op": "blq_rules", "mode": "positional", "concentration": "CONC", "time": "TIME", "blq_column": "BLQ", "pre_rule": "zero"},
    {"op": [secret]},
    secret,
]
for step in valid:
    assert module.validate_preparation([step]) is None, step["op"]
for step in invalid:
    error = module.validate_preparation([step])
    assert error["error"]["code"] == "E_VALIDATION"
    assert secret not in json.dumps(error)

async def live_stdio():
    with tempfile.TemporaryDirectory(prefix="nca018-client-") as folder:
        runtime = Path(folder) / ".pkapp-runtime.json"
        records = []
        info = {"api_port": 0, "token": "b" * 64, "instance_id": "contract-test", "pid": 12345,
                "mode": "nca", "project": str(Path(folder).resolve()),
                "app_version": module.APP_VERSION, "contract_version": module.CONTRACT_VERSION}

        class Handler(BaseHTTPRequestHandler):
            def log_message(self, *args):
                pass

            def do_GET(self):
                self.send_response(200)
                self.end_headers()
                self.wfile.write(json.dumps(info).encode())

            def do_POST(self):
                body = json.loads(self.rfile.read(int(self.headers["Content-Length"])))
                records.append((self.path, body, self.headers.get("X-PKApp-Contract")))
                self.send_response(200)
                self.end_headers()
                self.wfile.write(json.dumps({"ok": True, "result": {"mode": "nca"}}).encode())

        server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
        info["api_port"] = server.server_port
        runtime.write_text(json.dumps(info))
        runtime.chmod(0o600)
        thread = Thread(target=server.serve_forever, daemon=True)
        thread.start()
        try:
            params = StdioServerParameters(command=sys.executable, args=[str(script), "--mode", "nca", "--runtime", str(runtime)])
            async with stdio_client(params) as (read, write):
                async with ClientSession(read, write, client_info=types.Implementation(name="Actual contract client", version="18.2")) as session:
                    await session.initialize()
                    tools = {t.name: t for t in (await session.list_tools()).tools}
                    schema = tools["pk_prepare_data"].inputSchema
                    assert len(schema["properties"]["steps"]["items"]["oneOf"]) == 9
                    assert "_mcp_client" not in schema["properties"]
                    for step in invalid:
                        answer = await session.call_tool("pk_prepare_data", {"source": "table-test", "steps": [step], "name": "test", "base_revision": 1, "project_id": "project", "actor": "declared"})
                        payload = answer.structuredContent or json.loads(answer.content[0].text)
                        assert payload["error"]["code"] == "E_VALIDATION"
                        assert secret not in json.dumps(payload)
                    answer = await session.call_tool("pk_prepare_data", {"source": "table-test", "steps": secret, "name": "test", "base_revision": 1, "project_id": "project", "actor": "declared"})
                    payload = answer.structuredContent or json.loads(answer.content[0].text)
                    assert payload["error"]["code"] == "E_VALIDATION" and secret not in json.dumps(payload)
                    assert not records
                    await session.call_tool("pk_get_status", {})
                    path, body, header = records[-1]
                    assert path == "/v1/status" and header == module.CONTRACT_VERSION
                    assert body["_mcp_client"] == {"name": "Actual contract client", "version": "18.2", "source": "mcp_initialization_self_reported"}
        finally:
            server.shutdown()
            server.server_close()
            thread.join()

asyncio.run(live_stdio())
print("NCA018 schemas: 10 valid and 15 invalid cases; real stdio initialized-client metadata and safe filter errors: PASS")
