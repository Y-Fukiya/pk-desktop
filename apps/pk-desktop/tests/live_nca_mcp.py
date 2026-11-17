"""Start NCA with an EMPTY dedicated test project; checks real stdio and loopback."""
import argparse
import asyncio
import datetime
import csv
import json
from pathlib import Path
import sys
import tempfile
import traceback
from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client

async def run(project):
    server = Path(__file__).resolve().parents[1] / "mcp_server.py"
    params = StdioServerParameters(command=sys.executable, args=[str(server), "--mode", "nca", "--runtime", str(project / ".pkapp-runtime.json")])
    input_path = None
    try:
        async with stdio_client(params) as (read, write):
            async with ClientSession(read, write) as session:
                await session.initialize()
                async def call(name, args):
                    response = await session.call_tool(name, args)
                    if response.isError:
                        raise RuntimeError("; ".join(item.text for item in response.content if hasattr(item, "text")))
                    payload = response.structuredContent or json.loads(response.content[0].text)
                    if not payload.get("ok"):
                        raise RuntimeError(payload)
                    return payload["result"]
                status = await call("pk_get_status", {})
                assert status["mode"] == "nca"
                assert status["contract_version"] == "pkapp-nca-18"
                if await call("pk_list_tables", {}):
                    raise RuntimeError("Use an EMPTY dedicated test project; existing tables will not be changed.")
                with tempfile.NamedTemporaryFile(mode="w", suffix=".csv", dir=project, delete=False) as file:
                    file.write((server.parent / "examples/nca_actual_time_example.csv").read_text().replace("001", "PRIVATE_MCP_TEST"))
                    input_path = Path(file.name)
                scope = {"project_id": status["project_id"], "base_revision": status["revision"], "actor": "integration test"}
                table = await call("pk_import_table", {**scope, "path": str(input_path), "name": "TEST ONLY NCA018 input"})
                for invalid in [
                    {"op":"filter", "column":"ID", "operator":"eq", "value":"DO_NOT_ECHO_PRIVATE_VALUE"},
                    {"op":"filter", "column":"ID", "operator":"invalid_operator", "values":["DO_NOT_ECHO_PRIVATE_VALUE"]}
                ]:
                    response = await session.call_tool("pk_prepare_data", {**scope, "source":table["id"], "steps":[invalid], "name":"invalid"})
                    error = response.structuredContent or json.loads(response.content[0].text)
                    assert error["error"]["code"] == "E_VALIDATION", error
                    assert "DO_NOT_ECHO_PRIVATE_VALUE" not in json.dumps(error)
                filtered = await call("pk_prepare_data", {**scope,"source":table["id"],"steps":[{"op":"filter","column":"ID","operator":"eq","values":["PRIVATE_MCP_TEST"]}],"name":"TEST ONLY NCA018 filtered single profile"})
                assert filtered["counts"]["result"] == 6
                steps = [{"op": "elapsed_time", "sample_column": "SAMPLE_DTM", "dose_column": "DOSE_DTM", "utc_offset": "+09:00"},
                         {"op": "blq_rules", "mode": "positional", "profile_columns": ["ID", "ANALYTE", "PERIOD"],
                          "concentration": "CONC", "time": "NCA_TIME_H", "blq_column": "BLQ", "mdv_column": "MDV",
                          "flag_convention": "y_blank", "zero_time": "pre", "pre_rule": "zero", "first_rule": "zero",
                          "middle_rule": "exclude", "last_rule": "exclude", "all_blq_rule": "exclude",
                          "lloq_source": "column", "lloq_column": "LLOQ"}]
                prepared = await call("pk_prepare_data", {**scope, "source": filtered["id"], "steps": steps, "name": "TEST ONLY NCA018 prepared"})
                state = await call("pk_nca_configure", {**scope, "reason": "Artificial acceptance example", "source": prepared["id"],
                    "specification": {"id": "ID", "groups": ["ANALYTE", "PERIOD"], "time": "NCA_TIME_H", "concentration": "NCA_CONC",
                    "unit_column": "UNIT", "blq_column": "NCA_BLQ", "mdv_column": "NCA_MDV", "blq": "as_recorded",
                    "c0_method": "zero", "auc_method": "linear", "predose_qc_threshold_pct": 5, "predose_qc_window_h": 1}})
                scope["base_revision"] = state["revision"]
                result = await call("pk_nca_run", {**scope, "reason": "integration test"})
                assert result["is_current"] and "PRIVATE_MCP_TEST" not in json.dumps(result)
                assert all(not any(key in p for key in ("mean", "min", "max")) for p in result["parameters"])
                assert all(p["n"] == 1 for p in result["parameters"])
                assert result["qc_status"]["pass_blq"] == 1
                readback = await call("pk_nca_get_result", {"project_id": scope["project_id"], "result_id": result["result_id"]})
                assert readback["is_current"] and readback["result_id"] == result["result_id"]
                saved = await call("pk_nca_save_result", {**scope, "reason": "Preserve acceptance evidence", "result_id": result["result_id"],
                                                        "name": "TEST ONLY NCA018 result"})
                assert saved["provenance"]["execution"]["result_id"] == result["result_id"]
                assert all(not any(key in p for key in ("mean", "min", "max")) for p in readback["parameters"])
                with (project / "prepared" / saved["id"] / "result.csv").open() as f:
                    local_values = {row["PARAMCD"]:float(row["AVAL"]) for row in csv.DictReader(f)}
                assert local_values == {"CMAX":8,"TMAX":1,"AUCLST":31.5}
                missing = await session.call_tool("pk_nca_save_result", {**scope,"reason":"Missing result check","result_id":"nca-000000000000000000000000","name":"missing"})
                missing_payload = missing.structuredContent or json.loads(missing.content[0].text)
                assert missing_payload["error"]["code"] == "E_NOT_FOUND"
                return {"status": "passed", "result": result, "saved_table_id": saved["id"],
                        "local_synthetic_csv_values":local_values,"default_numeric_disclosure":"blocked, including single-profile filtered input",
                        "invalid_filter_safe_errors":"passed", "unknown_result_save":"E_NOT_FOUND",
                        "retained_test_tables": [table["id"], filtered["id"], prepared["id"], saved["id"]],
                        "cleanup": "After closing NCA, remove the dedicated test project if no longer needed."}
    finally:
        if input_path is not None:
            input_path.unlink(missing_ok=True)

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--project", required=True, type=Path)
parser.add_argument("--output-json", required=True, type=Path)
args = parser.parse_args()
if args.output_json.exists():
    parser.error("--output-json must be new")
report = {"scope": "real stdio -> authenticated loopback -> R", "at": datetime.datetime.now(datetime.timezone.utc).isoformat()}
try:
    report.update(asyncio.run(run(args.project.resolve(strict=True))))
except Exception as exc:
    report.update(status="failed", error=str(exc))
    traceback.print_exception(type(exc), exc, exc.__traceback__)
finally:
    args.output_json.write_text(json.dumps(report, ensure_ascii=False, indent=2) + "\n")
print(f"Live NCA MCP: {report['status']}; {args.output_json}")
sys.exit(0 if report["status"] == "passed" else 1)
