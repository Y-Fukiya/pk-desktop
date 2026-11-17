#!/bin/bash
set -u
desktop_dir="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)" || exit 1
exec /bin/bash "$desktop_dir/launch_macos.command" --mode nca "$@"
