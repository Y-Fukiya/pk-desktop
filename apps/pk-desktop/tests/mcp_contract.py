"""Read-only stdio MCP contract test; no R listener, code execution or path probes."""
import asyncio
import json
from pathlib import Path
import sys
import tempfile
from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client


async def main():
    script = Path(__file__).resolve().parents[1] / "mcp_server.py"
    with tempfile.TemporaryDirectory(prefix="pkapp-contract-") as tmp:
        params = StdioServerParameters(command=sys.executable, args=[str(script), "--runtime", str(Path(tmp) / "not-running.json")])
        async with stdio_client(params) as (read, write):
            async with ClientSession(read, write) as session:
                await session.initialize()
                listing = await session.list_tools()
                names = {t.name for t in listing.tools}
                assert names == {"pk_get_status", "pk_get_spec", "pk_update_spec", "pk_run", "pk_get_result", "pk_list_runs",
                                 "pk_export_report", "pk_get_job", "pk_list_jobs", "pk_cancel_job",
                                 "pk_describe_data", "pk_list_tables", "pk_prepare_data", "pk_join_data", "pk_load_data", "pk_replay_run", "pk_get_run_manifest", "pk_list_modules", "pk_add_module", "pk_remove_module", "pk_import_table", "pk_validate_analysis_tables", "pk_apply_analysis_tables"}
                run = next(t for t in listing.tools if t.name == "pk_run")
                assert set(run.inputSchema["required"]) == {"base_revision", "project_id"}
                export = next(t for t in listing.tools if t.name == "pk_export_report")
                assert set(export.inputSchema["required"]) == {"run_id", "project_id", "actor", "reason"}
                cancel = next(t for t in listing.tools if t.name == "pk_cancel_job")
                assert set(cancel.inputSchema["required"]) == {"job_id", "project_id"}
                for name in ["pk_get_status", "pk_get_spec", "pk_list_runs", "pk_list_jobs"]:
                    result = await session.call_tool(name, {})
                    payload = result.structuredContent
                    if payload is None:
                        payload = json.loads(next(c.text for c in result.content if c.type == "text"))
                    assert payload["error"]["code"] == "E_NOT_RUNNING"
    print("MCP stdio handshake, twenty-three tool schemas and E_NOT_RUNNING: PASS")


asyncio.run(main())
