"""Configure TealFlowMCP alongside the PK MCP and prepare a PK-aware handoff."""
from __future__ import annotations

import argparse
import asyncio
import csv
import hashlib
import importlib.metadata
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
VERSION = "0.2.0"
DEFAULT_PYTHON = ROOT / ".runtime/tealflow/bin/python"


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def client_config(tealflow_python: Path, pk_python: Path, root: Path = ROOT) -> dict:
    # absolute() preserves the venv executable; resolve() would bypass the venv.
    rscript = shutil.which("Rscript")
    env = {"PK_REVIEW_ROOT": str(root), "PK_REVIEW_PYTHON": str(pk_python.absolute()),
           "R_LIBS_USER": str(root / ".runtime/R-library")}
    if rscript:
        env["PATH"] = str(Path(rscript).parent) + os.pathsep + os.environ.get("PATH", "")
    return {"mcpServers": {
        "pkdummy-review": {"command": str(pk_python.absolute()),
                           "args": [str(root / "apps/pk-review/mcp_server.py"), "--workspace", str(root)],
                           "env": env},
        "tealflow-mcp": {"command": str(tealflow_python.absolute()),
                         "args": [str(HERE / "bridge.py"), "serve"], "env": env},
    }}


def runtime_info(python: Path) -> dict:
    code = ("import json, sys, importlib.metadata as m; "
            "print(json.dumps({'python': list(sys.version_info[:3]), "
            "'tealflow_mcp': m.version('tealflow-mcp'), 'mcp': m.version('mcp')}))")
    result = subprocess.run([str(python), "-c", code], capture_output=True, text=True, timeout=15)
    if result.returncode:
        raise ValueError("TealFlowMCP runtime is unavailable: " + result.stderr.strip())
    info = json.loads(result.stdout)
    if tuple(info["python"]) < (3, 11):
        raise ValueError("TealFlowMCP 0.2.0 requires Python >= 3.11; use a separate venv")
    if info["tealflow_mcp"] != VERSION or info["mcp"] != "1.26.0":
        raise ValueError("Install the versions in integrations/tealflow/requirements.txt")
    return info


def serve() -> None:
    if sys.version_info < (3, 11):
        raise ValueError("TealFlowMCP requires its separate Python >= 3.11 environment")
    if importlib.metadata.version("tealflow-mcp") != VERSION:
        raise ValueError("Expected tealflow-mcp==" + VERSION)
    if importlib.metadata.version("mcp") != "1.26.0":
        raise ValueError("Expected mcp==1.26.0")
    os.chdir(ROOT)
    from tealflow_mcp import main
    main()


async def probe(tealflow_python: Path, pk_python: Path) -> dict:
    """Exercise upstream discovery/code tools over stdio without opening a port."""
    info = runtime_info(tealflow_python)
    from anyio import fail_after
    from mcp import ClientSession, StdioServerParameters
    from mcp.client.stdio import stdio_client
    cfg = client_config(tealflow_python, pk_python)["mcpServers"]["tealflow-mcp"]
    params = StdioServerParameters(**cfg)
    with fail_after(60):
        async with stdio_client(params) as (read, write):
            async with ClientSession(read, write) as session:
                await session.initialize()
                names = sorted(t.name for t in (await session.list_tools()).tools)
                responses = {}
                for name, arguments in [
                    ("tealflow_agent_guidance", {}),
                    ("tealflow_list_modules", {"package": "general", "response_format": "json"}),
                    ("tealflow_get_module_details", {"module_name": "tm_g_scatterplot", "response_format": "json"}),
                    ("tealflow_generate_module_code", {"module_name": "tm_g_scatterplot"}),
                ]:
                    if name not in names:
                        raise ValueError(f"Expected upstream tool is missing: {name}")
                    result = await session.call_tool(name, arguments)
                    if result.isError:
                        raise ValueError(f"Upstream tool failed: {name}: {result.content}")
                    texts = [c.text for c in result.content if c.type == "text"]
                    if not texts or not any(t.strip() for t in texts):
                        raise ValueError(f"Upstream tool returned no content: {name}")
                    if name == "tealflow_list_modules":
                        modules = json.loads("\n".join(texts)).get("modules", [])
                        if not any(m.get("name") == "tm_g_scatterplot" for m in modules):
                            raise ValueError("Expected scatterplot module is absent from discovery")
                    if name in {"tealflow_get_module_details", "tealflow_generate_module_code"}:
                        if "tm_g_scatterplot" not in "\n".join(texts):
                            raise ValueError(f"Expected module missing from response: {name}")
                    responses[name] = texts
                return {"runtime": info, "tools": names, "responses": responses,
                        "patient_data_read": False, "shiny_startup_tested": False}


def handoff(view: Path, destination: Path) -> dict:
    sys.path.insert(0, str(ROOT))
    from tools import pk_review as review
    view = review.checked_path(view, ROOT)
    if destination.exists():
        raise ValueError("Use a new handoff directory; existing outputs are not overwritten")
    before = sha256(view)
    cfg = review.read_view(view)
    bundle = review.checked_path(cfg["bundle"], ROOT)
    preview = review.preview_view(view)
    if sha256(view) != before:
        raise ValueError("View changed during handoff preparation; retry")
    datasets = []
    for name, role in [("ADNCA", "concentrations"), ("ADPP", "precomputed PK parameters")]:
        path = review.checked_path(bundle / (name + ".csv"), ROOT)
        if sha256(path) != cfg["input_hashes"][path.name]:
            raise ValueError("Bundle changed during handoff preparation; prepare a new bundle")
        with path.open(encoding="utf-8-sig", newline="") as handle:
            columns = next(csv.reader(handle))
        datasets.append({"name": name, "role": role, "path": str(path), "columns": columns})
    metadata = {"version": 1, "purpose": "exploratory TealFlowMCP app development",
                "root": str(ROOT), "view": str(view), "view_sha256": before,
                "revision": cfg["revision"], "bundle": str(bundle),
                "input_hashes": cfg["input_hashes"], "datasets": datasets,
                "counts": preview["counts"], "tealflow_mcp_version": VERSION,
                "canonical_reports": "pk_preview then pk_export_report at the verified revision"}
    destination.mkdir(parents=True)
    (destination / "handoff.json").write_text(json.dumps(metadata, ensure_ascii=False, indent=2) + "\n")
    shutil.copyfile(HERE / "data.R", destination / "data.R")
    shutil.copyfile(HERE / "PROMPT.md", destination / "PROMPT.md")
    return {"directory": str(destination.absolute()), "revision": cfg["revision"],
            "files": ["handoff.json", "data.R", "PROMPT.md"], "raw_records_copied": False}


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--tealflow-python", type=Path, default=DEFAULT_PYTHON)
    parser.add_argument("--pk-python", type=Path, default=ROOT / ".venv/bin/python")
    commands = parser.add_subparsers(dest="command", required=True)
    for name in ["config", "check", "probe", "serve"]:
        commands.add_parser(name)
    command = commands.add_parser("handoff")
    command.add_argument("view", type=Path)
    command.add_argument("destination", type=Path)
    args = parser.parse_args()
    if args.command == "serve":
        serve()
        return
    if args.command == "config":
        result = client_config(args.tealflow_python, args.pk_python)
    elif args.command == "check":
        result = runtime_info(args.tealflow_python)
    elif args.command == "probe":
        result = asyncio.run(probe(args.tealflow_python, args.pk_python))
    else:
        result = handoff(args.view, args.destination)
    print(json.dumps(result, ensure_ascii=False, indent=2))


if __name__ == "__main__":
    try:
        main()
    except (ValueError, OSError, subprocess.TimeoutExpired) as exc:
        print(str(exc), file=sys.stderr)
        sys.exit(1)
