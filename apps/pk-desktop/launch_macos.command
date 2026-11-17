#!/bin/bash
# Starts the existing R GUI entrypoint. Does not install or stop other applications.
set -u
trap 'desktop_status=$?; if [ -t 0 ] && [ "${PK_DESKTOP_NO_PAUSE:-0}" != 1 ]; then read -r -p "Enterで閉じます。" desktop_answer; fi; exit "$desktop_status"' EXIT
desktop_dir="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)" || exit 1
. "$desktop_dir/utf8_locale.sh"
desktop_ensure_utf8_locale || exit 1
if [ "$(uname -s)" != Darwin ]; then echo "この起動コマンドはMacで実行してください。"; exit 1; fi
rscript_path="${PK_RSCRIPT:-}"
if [ -z "$rscript_path" ]; then
  for candidate in /Library/Frameworks/R.framework/Resources/bin/Rscript /usr/local/bin/Rscript /opt/homebrew/bin/Rscript "$(command -v Rscript || true)"; do
    if [ -n "$candidate" ] && [ -x "$candidate" ]; then rscript_path="$candidate"; break; fi
  done
fi
if [ -z "$rscript_path" ] || [ ! -x "$rscript_path" ]; then
  echo "Rscriptが見つかりません。PK_RSCRIPTにRscript実行ファイルのフルパスを指定してください。"
  exit 1
fi
echo "PK Desktopを起動します。ブラウザが開くまでこのターミナルを残してください。"
echo "終了するときは、このターミナルでControl+Cを押してください。ほかのRやアプリは停止しません。"
"$rscript_path" --vanilla "$desktop_dir/launch.R" "$@"
exit "$?"
