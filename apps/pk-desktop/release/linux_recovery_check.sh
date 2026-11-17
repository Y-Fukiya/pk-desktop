#!/usr/bin/env bash
set -euo pipefail
if [[ "$(uname -s)" != Linux ]]; then
  echo 'This check must run on Linux.' >&2
  exit 1
fi
cd "$(dirname "$0")/../../.."
Rscript --vanilla -e 'source("apps/pk-desktop/pkapp.R"); cat("R ", as.character(getRversion()), "; ps ", as.character(packageVersion("ps")), "\n", sep="")'
Rscript --vanilla apps/pk-desktop/tests/nca046_adpp_review.R
python3 apps/pk-desktop/tests/nca042_kill_restart.py
