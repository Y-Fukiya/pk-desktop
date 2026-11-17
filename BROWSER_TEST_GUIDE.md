# 人工データを使うブラウザー試験

リポジトリのルートで `bash apps/pk-desktop/release/run_browser_checks.sh` を実行すると、5 本のブラウザー試験を続けて実行します。試験は一時プロジェクトと人工データを作り、終了時に片付けます。R とアプリの依存パッケージ、Node.js、Playwright、Chrome または Playwright の Chromium が必要です。

macOS で Playwright を別フォルダへ用意し、インストール済みの Google Chrome を使う例です。既に Playwright がある場合はその場所を `NODE_PATH` に指定してください。

```sh
pk_browser_deps="$(mktemp -d)"
npm install --prefix "$pk_browser_deps" --no-save --no-package-lock playwright@1.65.1
export NODE_PATH="$pk_browser_deps/node_modules"
export PK_BROWSER_EXECUTABLE="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
bash apps/pk-desktop/release/run_browser_checks.sh
```

R パッケージをユーザー用ライブラリに置いた場合は、実行前に `R_LIBS_USER` にそのフォルダを指定します。Playwright 同梱の Chromium を使う場合は `PK_BROWSER_EXECUTABLE` を外し、Playwright の手順で Chromium をインストールしてください。Linux コンテナなどで必要な場合のみ `PK_BROWSER_ARGS='["--no-sandbox"]'` を指定できます。

`verify_release.py --with-browser` でも、R・Python 等の試験に続けて同じ 5 本を実行できます。GitHub Actions の設定は同梱していますが、0.61 の Actions 実行結果は未確認です。
