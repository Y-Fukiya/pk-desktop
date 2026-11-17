"""Verify an artificial two-period crossover via stdio, saved values and replay."""
import argparse
import asyncio
import csv
import json
import math
import os
from pathlib import Path
import shutil
import subprocess
import sys
from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client


async def run(project):
    app = Path(__file__).resolve().parents[1]
    source = app.parents[1]
    fixture = app / "examples/nca_crossover"
    params = StdioServerParameters(command=sys.executable, args=[str(app / "mcp_server.py"),
        "--mode", "nca", "--runtime", str(project / ".pkapp-runtime.json")])
    async with stdio_client(params) as (read, write):
        async with ClientSession(read, write) as session:
            await session.initialize()

            async def call(name, arguments):
                response = await session.call_tool(name, arguments)
                payload = response.structuredContent or json.loads(response.content[0].text)
                if response.isError or not payload.get("ok"):
                    raise RuntimeError(str(payload))
                return payload["result"]

            status = await call("pk_get_status", {})
            assert status["contract_version"] == "pkapp-nca-18"
            await call("pk_nca_get_spec", {})
            assert not await call("pk_list_tables", {}), "Use an empty dedicated test project"
            scope = dict(project_id=status["project_id"], base_revision=status["revision"], actor="synthetic crossover test")
            ids = {}
            for role, filename in dict(conc="conc.csv", pc="PC.csv", ex="EX.csv").items():
                target = project / filename
                assert not target.exists()
                shutil.copyfile(fixture / filename, target)
                ids[role] = (await call("pk_import_table", {**scope, "path":str(target), "name":"TEST ONLY " + role}))["id"]
            assembled = await call("pk_assemble_nca", {**scope, **ids,
                "specification":json.loads((fixture / "assembly.json").read_text()),
                "name":"TEST ONLY crossover assembly", "reason":"Synthetic two-period acceptance"})
            assert assembled["counts"]["result"] == 36
            scope["base_revision"] = assembled["revision"]
            prepared = await call("pk_prepare_data", {**scope, "source":assembled["id"], "name":"TEST ONLY crossover BLQ", "steps":[{
                "op":"blq_rules", "mode":"positional", "profile_columns":["STUDYID","ID","PERIOD","ANALYTE"],
                "concentration":"CONC", "time":"NCA_TIME_H", "blq_column":"BLQ", "mdv_column":"MDV", "zero_time":"pre",
                "pre_rule":"zero", "first_rule":"zero", "middle_rule":"exclude", "last_rule":"exclude", "all_blq_rule":"exclude",
                "lloq_source":"column", "lloq_column":"LLOQ"}]})
            specification = json.loads((fixture / "nca.json").read_text())
            configured = await call("pk_nca_configure", {**scope, "source":prepared["id"], "specification":specification,
                "reason":"Confirmed synthetic washout and per-period QC"})
            scope["base_revision"] = configured["revision"]
            answer = await call("pk_nca_run", {**scope, "reason":"Synthetic independent-period NCA"})
            assert answer["is_current"]
            assert all(not any(k in p for k in ("mean","min","max")) for p in answer["parameters"])
            saved = await call("pk_nca_save_result", {**scope, "result_id":answer["result_id"],
                "name":"TEST ONLY crossover NCA", "reason":"Inspect locally and replay"})
            with (project / "prepared" / saved["id"] / "result.csv").open() as stream:
                values = list(csv.DictReader(stream))
            expected = json.loads((fixture / "expected.json").read_text())
            for profile in expected:
                actual = {row["PARAMCD"]:float(row["AVAL"]) for row in values
                    if row["ID"] == profile["ID"] and row["PERIOD"] == profile["PERIOD"]}
                for parameter, target in profile.items():
                    if parameter not in ("ID","PERIOD"):
                        assert math.isclose(actual[parameter], target, rel_tol=1e-10, abs_tol=1e-10), parameter
            trace = json.loads((project / "prepared" / saved["id"] / "trace.json").read_text())
            assert all(g["predose_qc"]["status"] == "pass_blq" for g in trace["groups"])
            specification["washout_attestation"] = "unconfirmed"
            configured = await call("pk_nca_configure", {**scope, "source":prepared["id"], "specification":specification,
                "reason":"Verify prior-dose gating"})
            scope["base_revision"] = configured["revision"]
            denied = await call("pk_nca_run", {**scope, "reason":"Synthetic unconfirmed washout"})
            denied_saved = await call("pk_nca_save_result", {**scope, "result_id":denied["result_id"],
                "name":"TEST ONLY unconfirmed washout", "reason":"Check affected period locally"})
            with (project / "prepared" / denied_saved["id"] / "result.csv").open() as stream:
                denied_rows = list(csv.DictReader(stream))
            for row in denied_rows:
                if row["PARAMCD"] == "AUCIFO":
                    assert (row["AVAL"] in ("NA", "")) == (row["PERIOD"] == "2")
            replay = {}
            for label, table in dict(assembly=assembled, blq=prepared, nca=saved, unconfirmed=denied_saved).items():
                subprocess.run(["Rscript", "--vanilla", "-e",
                    'source("apps/pk-desktop/pkapp.R"); pkapp_replay(commandArgs(TRUE)[1])',
                    str(project / "prepared" / table["id"])], cwd=source, env=os.environ.copy(),
                    stdout=subprocess.DEVNULL, check=True)
                replay[label] = "passed"
            return dict(status="passed", app_version=status["app_version"], contract=status["contract_version"],
                tools=len((await session.list_tools()).tools), profiles=4, parameters_checked=28,
                expected_source="Closed-form exponential: AUCinf=84*scale; lambda=0.1; half-life=log(2)/0.1",
                washout_unconfirmed="Period 2 withheld; period 1 retained", default_disclosure="counts_only",
                predose_qc="pass_blq in all four profiles", replay=replay,
                cleanup="Dedicated TEST ONLY project; never select it for real analysis")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--project", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    if args.output.exists():
        parser.error("--output must be a new file")
    result = asyncio.run(run(args.project.resolve(strict=True)))
    args.output.write_text(json.dumps(result, ensure_ascii=False, indent=2) + "\n")
    print("Synthetic crossover MCP:", result["status"])
