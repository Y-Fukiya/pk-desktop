"""Register or explicitly update only the Codex pkapp_nca stdio entry."""
from __future__ import annotations

import argparse
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys

SERVER_NAME = "pkapp_nca"


def cli(codex: str, *arguments: str) -> subprocess.CompletedProcess:
    result = subprocess.run([codex, "mcp", *arguments], text=True, capture_output=True)
    if result.returncode:
        raise RuntimeError("Codex MCP command failed; inspect Codex configuration locally.")
    return result


def prior_stdio(entry: dict, project: Path) -> dict:
    transport = entry.get("transport", {})
    arguments = transport.get("args", [])
    recognized = (
        transport.get("type") == "stdio"
        and isinstance(transport.get("command"), str)
        and isinstance(arguments, list)
        and len(arguments) == 5
        and all(isinstance(x, str) for x in arguments)
        and Path(arguments[0]).name == "mcp_server.py"
        and arguments[1:4] == ["--mode", "nca", "--runtime"]
        and Path(arguments[4]).resolve() == project / ".pkapp-runtime.json"
        and not transport.get("env")
        and not transport.get("env_vars")
        and not transport.get("cwd")
        and entry.get("enabled", True) is True
        and not any(entry.get(key) is not None for key in ("startup_timeout_sec", "tool_timeout_sec", "enabled_tools", "disabled_tools"))
    )
    if not recognized:
        raise ValueError("Existing pkapp_nca is not a plain NCA stdio registration for this project. No changes made.")
    return {"command": transport["command"], "args": arguments}


def register(codex: str, desired: dict, project: Path, update: bool) -> str:
    project = project.resolve()
    entries = json.loads(cli(codex, "list", "--json").stdout)
    if not isinstance(entries, list):
        raise ValueError("Unexpected Codex server list; no changes made.")
    matches = [item for item in entries if item.get("name") == SERVER_NAME]
    if len(matches) > 1:
        raise ValueError("Duplicate pkapp_nca registrations; no changes made.")
    entry = json.loads(cli(codex, "get", SERVER_NAME, "--json").stdout) if matches else None
    previous = prior_stdio(entry, project) if entry is not None else None
    if previous == desired:
        return "already_current"
    if previous is not None and not update:
        raise ValueError("pkapp_nca is already registered. Use --update to change this project's registration.")
    if previous is not None:
        cli(codex, "remove", SERVER_NAME)
    try:
        cli(codex, "add", SERVER_NAME, "--", desired["command"], *desired["args"])
    except (RuntimeError, OSError):
        if previous is not None:
            try:
                cli(codex, "add", SERVER_NAME, "--", previous["command"], *previous["args"])
            except (RuntimeError, OSError) as rollback:
                raise RuntimeError("Registration and restoration failed. Re-register the previous pkapp_nca configuration locally.") from rollback
        raise RuntimeError("Registration failed. Previous pkapp_nca was restored; unrelated servers were not changed." if previous else "Registration failed; no existing server was changed.")
    return "updated" if previous else "registered"


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--project", type=Path, required=True)
    parser.add_argument("--python", type=Path, required=True)
    parser.add_argument("--codex", default=os.environ.get("PK_CODEX_CLI") or shutil.which("codex"))
    parser.add_argument("--update", action="store_true", help="Explicitly replace an existing NCA registration for the same project")
    arguments = parser.parse_args()
    try:
        if not arguments.project.is_absolute() or not arguments.project.is_dir():
            raise ValueError("--project must be an existing absolute directory.")
        if not arguments.python.is_absolute() or not arguments.python.is_file() or not os.access(arguments.python, os.X_OK):
            raise ValueError("--python must be an absolute Python executable with mcp installed.")
        if not arguments.codex:
            raise ValueError("Codex CLI not found. Set --codex or PK_CODEX_CLI.")
        python = str(arguments.python.parent.resolve() / arguments.python.name)
        subprocess.run([python, "-c", "import mcp"], check=True, capture_output=True)
        project = arguments.project.resolve()
        desired = {"command": python, "args": [str(Path(__file__).with_name("mcp_server.py").resolve()),
                   "--mode", "nca", "--runtime", str(project / ".pkapp-runtime.json")]}
        result = register(arguments.codex, desired, project, arguments.update)
        print(json.dumps({"server": SERVER_NAME, "status": result, "next": "Start this release for the project, then open a new Codex session. Connection checks require matching MCP contract versions."}))
        return 0
    except (ValueError, RuntimeError, OSError, subprocess.CalledProcessError) as error:
        print(str(error) if not isinstance(error, subprocess.CalledProcessError) else "The selected Python environment cannot import mcp.", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
