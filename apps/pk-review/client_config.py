"""Print a stdio MCP client configuration for this checkout and Python interpreter."""
import json
from pathlib import Path
import sys

root = Path(__file__).resolve().parents[2]
print(json.dumps({"mcpServers": {"pkdummy-review": {
    "command": sys.executable,
    "args": [str(root / "apps/pk-review/mcp_server.py"), "--workspace", str(root)],
}}}, indent=2))
