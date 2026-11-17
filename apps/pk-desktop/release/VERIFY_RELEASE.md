# 配布物だけで行うNCA検証

`verify_release.py` は、このアーカイブに入っている試験と例題を使います。Rパッケージをインストールせず、HTTPサーバーも起動しません。先に `apps/pk-desktop/install.R` などで必要なRパッケージを用意してください。数値の参照版はPKNCA 0.12.1です。Pythonには `apps/pk-desktop/requirements-mcp.txt` の依存パッケージを導入してください。MCPの実stdio初期接続とツールの引数・設定生成も確認します。

展開後の `source` フォルダで実行します。

```sh
python3 apps/pk-desktop/release/verify_release.py \
  --rscript /absolute/path/to/Rscript \
  --output-dir /absolute/path/to/new-verification-results
```

`Rscript` がPATHにある場合は `--rscript` を省略できます。既存の結果は上書きしません。各試験の上限時間は180秒で、`--timeout` で変更できます。現在のUTF-8ロケールは保持します。Cロケールなどの場合はOSが提供するUTF-8ロケールに切り替え、利用できない場合は停止します。

実行対象は、前処理・NCA・科学検証・レビュー回帰・NCA画面のShinyサーバー試験、グリッド・作業画面・前処理画面の試験、CSV受入試験、Theoph公開データとの比較、位置別BLQの組合せ、保存と再実行の一連の処理、インストーラーのモック試験、起動時のロケール、配布ZIPの内容と再現性です。

すべての子プロセスが終了コード0で完了した場合だけ、全体を `passed` とします。失敗やタイムアウトは `verification.json` と個別ログに残り、コマンドの終了コードは1になります。依存パッケージが不足している試験を成功として扱いません。ログごとのSHA-256も記録します。

Shinyのサーバー試験では状態遷移と出力を確認します。ブラウザの実操作、画面配置、Tauriバイナリ、署名・公証、クリーン環境でのネットワーク復元、tealの全モジュール連携は、このコマンドの検証対象ではありません。

ローカルMCPのTCP通信を含む受入試験は別です。NCAを空の使い捨てプロジェクトで起動し、`tests/live_nca_mcp.py --project /absolute/test-project --output-json /absolute/new-result.json` をMCP依存パッケージを導入したPythonで実行します。詳しくは `integrations/NCA_MCP.md` を参照してください。

37薬剤を使う開発用ハーネスとそのMakefileは、この配布物には含まれません。`make harness-check` は完全な開発リポジトリ側で実行する確認です。配布物の結果と開発ハーネスの結果は、`VERIFICATION.md` で分けて記録します。
