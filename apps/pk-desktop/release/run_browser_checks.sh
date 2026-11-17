#!/usr/bin/env bash
# Run the synthetic browser suite from any working directory.
set -euo pipefail
script_dir="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
source_dir="$(CDPATH= cd -- "$script_dir/../../.." && pwd)"
cd "$source_dir"

if ! command -v node >/dev/null 2>&1; then
  printf '%s\n' 'Node.js is required to run the browser tests.' >&2
  exit 1
fi
if ! command -v Rscript >/dev/null 2>&1; then
  printf '%s\n' 'Rscript is required to run the browser tests.' >&2
  exit 1
fi
if ! node -e 'require("playwright")' >/dev/null 2>&1; then
  printf '%s\n' 'Playwright is unavailable. Install it in a separate directory and set NODE_PATH to that directory’s node_modules.' >&2
  exit 1
fi
if [[ -n "${PK_BROWSER_EXECUTABLE:-}" && ! -x "$PK_BROWSER_EXECUTABLE" ]]; then
  printf '%s\n' 'PK_BROWSER_EXECUTABLE does not point to an executable browser.' >&2
  exit 1
fi

for test_file in nca054_browser.cjs nca057_browser.cjs nca058_browser.cjs nca059_browser.cjs nca060_browser.cjs; do
  printf 'Running %s\n' "$test_file"
  node "apps/pk-desktop/tests/$test_file"
done
printf '%s\n' 'All synthetic browser checks passed.'
