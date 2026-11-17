"""NCA stdio handshake without a listener; does not claim live integration."""
import asyncio
import json
from pathlib import Path
import sys
import tempfile
from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client

async def main():
    script = Path(__file__).resolve().parents[1] / "mcp_server.py"
    with tempfile.TemporaryDirectory() as tmp:
        params = StdioServerParameters(command=sys.executable, args=[str(script), "--mode", "nca", "--runtime", str(Path(tmp) / "missing.json")])
        async with stdio_client(params) as (read, write):
            async with ClientSession(read, write) as session:
                await session.initialize()
                tools = {tool.name: tool for tool in (await session.list_tools()).tools}
                assert set(tools) == {"pk_get_status", "pk_describe_data", "pk_list_tables", "pk_import_table", "pk_prepare_data", "pk_join_data", "pk_assemble_nca", "pk_annotate_nca", "pk_prepare_nca_covariates", "pk_nca_get_spec", "pk_nca_configure", "pk_nca_run", "pk_nca_get_result", "pk_nca_save_result", "pk_nca_list_modules", "pk_nca_add_module", "pk_nca_remove_module", "pk_nca_explore_get", "pk_nca_explore_set"}
                assert set(tools["pk_nca_configure"].inputSchema["required"]) == {"source", "specification", "base_revision", "project_id", "actor", "reason"}
                response = await session.call_tool("pk_get_status", {})
                payload = response.structuredContent or json.loads(response.content[0].text)
                assert payload["error"]["code"] == "E_NOT_RUNNING"
    print("NCA stdio handshake, 19 schemas, no-listener response: PASS")
asyncio.run(main())
