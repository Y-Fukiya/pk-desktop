#!/usr/bin/env python3
"""Print a copyable local NCA MCP client configuration; never changes client settings."""
import argparse
import json
import os
from pathlib import Path

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--project", type=Path, required=True, help="Absolute, existing NCA project directory")
parser.add_argument("--python", type=Path, required=True, help="Absolute Python executable with mcp installed")
parser.add_argument("--format", choices=("codex", "json"), default="codex")
a = parser.parse_args()
if not a.project.is_absolute() or not a.project.is_dir():
    parser.error("--project must be an absolute existing directory")
if not a.python.is_absolute() or not a.python.is_file() or not os.access(a.python, os.X_OK):
    parser.error("--python must be an absolute existing executable")
# Resolve only the parent so virtual environment executable symlinks remain intact.
python = str(a.python.parent.resolve() / a.python.name)
script = str(Path(__file__).resolve().with_name("mcp_server.py"))
args = [script, "--mode", "nca", "--runtime", str(a.project.resolve() / ".pkapp-runtime.json")]
if a.format == "json":
    print(json.dumps({"mcpServers": {"pkapp_nca": {"command": python, "args": args}}}, indent=2))
else:
    print("[mcp_servers.pkapp_nca]")
    print("command = " + json.dumps(python))
    print("args = " + json.dumps(args))
