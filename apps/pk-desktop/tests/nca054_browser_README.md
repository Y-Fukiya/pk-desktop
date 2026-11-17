# 実ブラウザーの回帰試験

`nca054_browser.cjs` は新しい一時プロジェクトと人工データを使用し、認証情報を持たないShiny試験アプリを `127.0.0.1` で起動します。実際のプロジェクトや実行中のアプリには接続しません。Playwrightと対応するChromium、Rと本アプリの依存パッケージが必要です。

source直下で以下を実行します。`playwright` をNodeのモジュール検索パスで解決できる必要があります。独自のChromiumを使う場合は `PK_BROWSER_EXECUTABLE` に実行ファイルのパスを指定します。起動オプションは `PK_BROWSER_ARGS` にJSON文字列配列で指定できます。シェルのコマンド列として解釈しません。例： `PK_BROWSER_ARGS='["--no-sandbox"]' node apps/pk-desktop/tests/nca054_browser.cjs`。このオプションを使う場合はブラウザーのサンドボックスが無効になります。

```sh
node apps/pk-desktop/tests/nca054_browser.cjs
```

初期状態で注意が非表示・バーが縮小していること、期を一部選ぶと注意が見えて実行ボタンが無効になること、注意とボタンのアクセシビリティ上の関連、バーの伸縮中にボタンが動かないこと、全期を選ぶと元に戻ること、意図しないNCAが実行されないことを確認します。

この環境の制限により、今回の変更後のPlaywrightスクリプト自体の完走は未確認です。同じ人工Shiny試験アプリを既存ブラウザーで操作した結果は、配布物の `BROWSER_SYNTHETIC_RESULT.json` を参照してください。
