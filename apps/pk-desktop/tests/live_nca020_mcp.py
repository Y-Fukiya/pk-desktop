"""Exercise NCA then optional SAS DM/VS annotation on an empty, dedicated project."""
import argparse
import asyncio
import csv
import json
from pathlib import Path
import shutil
import subprocess
import sys
from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client

async def run(project):
    app = Path(__file__).resolve().parents[1]
    source = app.parents[1]
    example = app / "examples/nca_annotation"
    params = StdioServerParameters(command=sys.executable, args=[str(app / "mcp_server.py"), "--mode", "nca", "--runtime", str(project / ".pkapp-runtime.json")])
    async with stdio_client(params) as (read, write):
        async with ClientSession(read, write) as session:
            await session.initialize()
            async def call(name, args):
                response = await session.call_tool(name, args)
                payload = response.structuredContent or json.loads(response.content[0].text)
                if response.isError or not payload.get("ok"):
                    raise RuntimeError(str(payload))
                return payload["result"]
            status = await call("pk_get_status", {})
            assert status["contract_version"] == "pkapp-nca-18"
            assert not await call("pk_list_tables", {}), "Use an empty synthetic project"
            scope = {"project_id":status["project_id"], "base_revision":status["revision"], "actor":"synthetic test"}
            ids = {}
            for name, filename in {"input":"nca_input.csv", "dm":"DM.sas7bdat", "vs":"VS.sas7bdat"}.items():
                target = project / filename
                if target.exists():
                    raise RuntimeError("Synthetic input already exists")
                shutil.copyfile(example / filename, target)
                ids[name] = (await call("pk_import_table", {**scope, "path":str(target), "name":"TEST ONLY " + name}))["id"]
            configured = await call("pk_nca_configure", {**scope, "source":ids["input"], "reason":"Synthetic base NCA", "specification":{
                "id":"ID", "groups":["STUDYID", "PERIOD"], "time":"TIME", "concentration":"CONC", "concentration_unit":"ng/mL", "c0_method":"none", "auc_method":"linear"}})
            scope["base_revision"] = configured["revision"]
            result = await call("pk_nca_run", {**scope, "reason":"Synthetic base NCA"})
            nca = await call("pk_nca_save_result", {**scope, "result_id":result["result_id"], "name":"TEST ONLY NCA", "reason":"Freeze unannotated result"})
            specification = json.loads((example / "specification.json").read_text())
            args = {**scope, "result_source":nca["id"], "dm":ids["dm"], "vs":ids["vs"], "specification":specification, "name":"TEST ONLY background", "reason":"Recorded synthetic baseline values"}
            saved = await call("pk_annotate_nca", args)
            assert saved["counts"]["result"] == 12 and saved["counts"]["issues"] == 0
            assert not any(word in json.dumps(saved) for word in ("BASELINE", "19.5", "USUBJID"))
            stale_response = await session.call_tool("pk_annotate_nca", args)
            stale = stale_response.structuredContent or json.loads(stale_response.content[0].text)
            assert stale["error"]["code"] == "E_REVISION_CONFLICT"
            base = list(csv.DictReader((project / "prepared" / nca["id"] / "result.csv").open()))
            annotated = list(csv.DictReader((project / "prepared" / saved["id"] / "result.csv").open()))
            assert len(base) == len(annotated) == 12
            assert all(all(row[k] == original[k] for k in original) for row, original in zip(annotated, base))
            assert {row["VS_BMI"] for row in annotated if row["ID"] == "001"} == {"19.5"}
            assert all(row["VS_BMI_VISIT"] == "BASELINE" for row in annotated)
            script = "source(commandArgs(TRUE)[1]); p <- commandArgs(TRUE)[2]; x <- pkapp_replay(p); stopifnot(nrow(x$data) == 12); cat('REPLAY PASS\\n')"
            replay = subprocess.run(["Rscript", "--vanilla", "-e", script, str(app / "pkapp.R"), str(project / "prepared" / saved["id"])], cwd=source, text=True, capture_output=True, check=True)
            assert "REPLAY PASS" in replay.stdout
            return {"status":"passed", "app_version":status["app_version"], "contract":status["contract_version"], "tools":len((await session.list_tools()).tools),
                    "pk_rows_unchanged":12, "profiles":4, "sas_inputs":["DM", "VS"], "selected_visit":"BASELINE", "synthetic_bmi_001":19.5,
                    "default_disclosure":"manifest and counts only", "stale_revision":"rejected", "replay":"passed", "annotation_table_id":saved["id"],
                    "cleanup":"Dedicated synthetic project retained for inspection; do not use it for analysis."}

def parse_args(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("project_legacy", nargs="?", type=Path, help="Legacy positional project path")
    parser.add_argument("--project", type=Path, help="Empty dedicated synthetic project")
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args(argv)
    if (args.project is None) == (args.project_legacy is None):
        parser.error("Specify exactly one project: --project PATH (or the legacy positional path)")
    args.project = (args.project or args.project_legacy).resolve(strict=True)
    if args.output.exists():
        parser.error("--output must be a new file")
    return args


if __name__ == "__main__":
    args = parse_args()
    answer = asyncio.run(run(args.project))
    args.output.write_text(json.dumps(answer, ensure_ascii=False, indent=2) + "\n")
    print(json.dumps(answer, ensure_ascii=False))
