"""Isolated single-period configuration publishes one strict MCP/R contract."""
import asyncio
import importlib.util
import json
from pathlib import Path
import sys
from jsonschema import Draft202012Validator
from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client

script = Path(__file__).resolve().parents[1] / "mcp_server.py"
spec = importlib.util.spec_from_file_location("nca021_mcp", script)
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)
base = {"id": "ID", "time": "TIME", "concentration": "CONC"}
valid = {**base, "terminal_method": "auto", "dosing_context": "isolated_single_period",
         "washout_attestation": "protocol_confirmed", "dose_history_confirmed": True}
assert module.validate_nca(base) is None
assert module.validate_nca(valid) is None
for context in ("single", "repeated_final", "ongoing"):
    assert module.validate_nca({**base, "dosing_context": context}) is None
for field in ("end", "predose_window_h", "predose_qc_threshold_pct", "predose_qc_window_h",
              "terminal_start_h", "terminal_end_h", "predose_qc_lloq_value", "pknca_version", "groups"):
    assert module.validate_nca({**valid, field: None}) is None, field
for groups in ([], ["PERIOD"], "PERIOD"):
    assert module.validate_nca({**base, "groups": groups}) is None
secret = "DO_NOT_ECHO_PRIVATE_021"
invalid = [None, [], {}, {**valid, "unknown": secret}, {**valid, "dosing_context": secret},
           {**valid, "washout_attestation": secret}, {**valid, "dose_history_confirmed": "yes"},
           {**valid, "terminal_method": "guess"}, {**valid, "groups": ["PERIOD", "PERIOD"]}]
for item in invalid:
    answer = module.validate_nca(item)
    assert answer["error"]["code"] == "E_VALIDATION"
    assert secret not in json.dumps(answer)
server = module.create_nca_server(Path("/nonexistent"))
tools = server._tool_manager._tools
args = {"source": "table-" + "a"*24, "specification": valid, "project_id": "p",
        "base_revision": 1, "actor": "test", "reason": "test"}
assert not list(Draft202012Validator(tools["pk_nca_configure"].parameters).iter_errors(args))
assert tools["pk_nca_configure"].parameters["properties"]["specification"]["additionalProperties"] is False
assert "isolated_single_period" in tools["pk_nca_configure"].description
assert "washout_attestation" in tools["pk_nca_configure"].description

async def check():
    params = StdioServerParameters(command=sys.executable, args=[str(script), "--mode", "nca", "--runtime", "/nonexistent"])
    async with stdio_client(params) as (read, write):
        async with ClientSession(read, write) as session:
            await session.initialize()
            listed = {tool.name: tool for tool in (await session.list_tools()).tools}
            published = listed["pk_nca_configure"].inputSchema["properties"]["specification"]
            assert published["properties"]["washout_attestation"]["enum"] == ["unconfirmed", "protocol_confirmed"]
            for item in invalid[3:]:
                response = await session.call_tool("pk_nca_configure", {**args, "specification": item})
                payload = response.structuredContent or json.loads(response.content[0].text)
                assert payload["error"]["code"] == "E_VALIDATION"
                assert secret not in json.dumps(payload)

asyncio.run(check())
print("NCA021 strict schema, legacy optional/null compatibility, and real stdio validation passed")
