#!/bin/sh
set -eu
cd "$(dirname "$0")/../.."
exec .venv/bin/python apps/pk-review/demo.py --scenario repeated --name local-reviewed --launch
