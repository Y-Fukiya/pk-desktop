"""The annotation tool publishes a strict schema and safe pre-connection validation."""
import asyncio
import importlib.util
import json
from pathlib import Path
import sys
from jsonschema import Draft202012Validator
from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client

script = Path(__file__).resolve().parents[1] / "mcp_server.py"
spec = importlib.util.spec_from_file_location("nca020_mcp", script)
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)
valid = {"version": 1, "dm": {"keys": {"ID": "USUBJID"}, "columns": {"DM_SEX": "SEX"}}}
assert module.validate_annotation(valid) is None
secret = "DO_NOT_ECHO_PRIVATE_020"
invalid = [None, [], {"version": 1}, {**valid, "unknown": secret}, {"version": 1, "dm": {"keys": {}, "columns": {secret: "SEX"}}}]
server = module.create_nca_server(Path("/nonexistent"))
tools = server._tool_manager._tools
assert len(tools) == 19
args = {"result_source":"table-" + "a" * 24, "specification":valid,"name":"test","project_id":"p","base_revision":1,"actor":"test","reason":"test"}
assert not list(Draft202012Validator(tools["pk_annotate_nca"].parameters).iter_errors(args))

async def check():
    params = StdioServerParameters(command=sys.executable, args=[str(script), "--mode", "nca", "--runtime", "/nonexistent"])
    async with stdio_client(params) as (read, write):
        async with ClientSession(read, write) as session:
            await session.initialize()
            listed = {tool.name: tool for tool in (await session.list_tools()).tools}
            assert len(listed) == 19 and "pk_annotate_nca" in listed
            for item in invalid:
                response = await session.call_tool("pk_annotate_nca", {**args, "specification":item})
                payload = response.structuredContent or json.loads(response.content[0].text)
                assert payload["error"]["code"] == "E_VALIDATION"
                assert secret not in json.dumps(payload)

asyncio.run(check())
print("NCA020 annotation schema and real stdio validation passed")
