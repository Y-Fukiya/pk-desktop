# TealFlowMCP と PK Review の併用

TealFlowMCP 0.2.0 を汎用tealモジュールの探索・Rコード生成に使い、PK専用MCPを保存設定・QC・帳票生成に使います。両サーバーを同じMCPクライアントに登録します。TealFlowMCPはPK Review画面の操作APIではありません。

## インストール

リポジトリのルートから実行します。TealFlowMCP 0.2.0のpyprojectはPython **3.11以上**を要求するため、既存PK環境（3.10）とは別の環境を作ります。Rは既存のローカル環境を利用します。

```sh
python3.12 -m venv .runtime/tealflow
.runtime/tealflow/bin/python -m pip install -r apps/pk-review/integrations/tealflow/requirements.txt
.venv/bin/python apps/pk-review/integrations/tealflow/bridge.py check
.venv/bin/python apps/pk-review/integrations/tealflow/bridge.py config
```

`config` のJSONをstdio対応MCPクライアントへ追加して再起動してください。このコマンドは設定ファイルを自動変更せず、サーバーも起動しません。クライアントによって外側の `mcpServers` の形式は調整が必要です。別のPythonを使う場合はサブコマンドの前に `--tealflow-python /absolute/path/to/python` を指定します。

2026-10-29に、このMacのCodexへ `tealflow-mcp` を登録しました。登録設定を `codex mcp get tealflow-mcp --json` で読み戻し、その実コマンド・引数・環境変数によるMCP通信を確認しています。PK専用の `pkdummy-review` は今回登録していません。実行中チャットのツール一覧にはまだTealFlowMCPが現れておらず、その画面からの直接呼出しは未確認です。設定のMCP serversからRestartして反映を確認してください。[Codexの公式設定手順](https://learn.chatgpt.com/docs/extend/mcp?surface=cli)

接続の読取専用確認は次で行えます。ポートを開かず、公式MCPのinitialize、tools/list、guidance、汎用モジュール一覧、散布図詳細、コードひな形取得を実行します。患者データは読みません。

```sh
.venv/bin/python apps/pk-review/integrations/tealflow/bridge.py probe
```

## PKデータを引き継ぐ

```sh
.venv/bin/python apps/pk-review/integrations/tealflow/bridge.py handoff \
  outputs/pk-review/views/round8-fixed-repeated.json \
  outputs/pk-review/tealflow/exploration-01
```

新しいフォルダへ `handoff.json`（列定義・入力チェックサム・revision・件数）、`data.R`（既存の取込・集計規則を使うR読込関数）、`PROMPT.md`（TealFlowMCPへの依頼）を出力します。生のレコードはコピーしません。既存フォルダは上書きしません。

クライアントへ、出力フォルダの `PROMPT.md` に従って探索用アプリを作るよう指示してください。例えば「ADPPのCMAXSSについて性別ごとの分布図を追加し、単位を分けて表示して」と依頼できます。必要な列・PARAMCDがあることを先に確認します。生成コードのテンプレートにあるADSL等の名前をPKデータへ適切に修正する必要があり、TealFlowMCP自身はADNCA/ADPPのPK仕様を検証しません。

生成アプリでは `source('data.R'); pk <- load_pk_handoff('handoff.json')` とし、`pk$ADNCA` と `pk$ADPP` を使用します。これは現在の保存済みPK設定に従うデータで、ADPPを再計算しません。元のviewまたは入力の変更は起動時に検出して停止します。変更後は新しいhandoffを作ってください。追加の探索フィルターは元の保存設定と同期しません。

Word・HTML帳票はPK専用MCPの `pk_preview` と `pk_export_report` から出力します。探索アプリの追加グラフはそのまま標準PK帳票に入るわけではありません。TealFlowMCPの `setup_renv_environment` は探索用フォルダの絶対パスだけに適用し、元のPK Review環境を変更しないでください。

## 確認範囲

公式ソースの起動点・14ツール・引数を確認済み。固定参照は `1e2b752559c844f5fac29d4c7a0a97969e551239`、パッケージ版は0.2.0です。公式READMEはPython 3.10+と記載しますが、pyprojectの3.11以上を採用しました。また公式TOOLS.mdの一部引数表はserver.pyと不一致なので、本接続はserver.pyの署名を使っています。

2026-10-28には名前解決の失敗で導入が停止していました。2026-10-29には公式PyPIからの導入と `pip check` が成功しました。専用環境はPython 3.12.2、TealFlowMCP 0.2.0、MCP SDK 1.26.0です。

実サーバーのinitialize、14ツールの一覧取得、汎用15モジュールの一覧、`tm_g_scatterplot` の詳細とRコードひな形取得に成功しました。Codex登録設定を使った別プロセスでも同じ4ツールの呼出しが成功しています。接続設定生成、版不一致の拒否、既存R処理とのデータ・集計件数一致、変更済みviewの拒否、上書き拒否を検査する既存3テストも成功しました。

保存済み反復投与デモの受渡しは `outputs/pk-review/tealflow/connected-20261029/` に準備済みです。revision 2、50例、濃度450行、選択パラメータ150行を参照し、生のレコードはコピーしていません。

実行中のCodexチャットからの直接ツール呼出しと、新しい探索アプリのブラウザ動作は未確認です。サーバー起動時にはPydantic Settingsの `lifespan` 型定義に関する警告が出ますが、今回確認したMCP通信は成功しています。検証ログと対象範囲は [検証記録](VERIFICATION.md) にまとめています。

```sh
.venv/bin/python -m unittest discover -s apps/pk-review/integrations/tealflow/tests -p 'test_*.py' -v
```

公式ソース:

- https://github.com/Appsilon/TealFlowMCP/blob/1e2b752559c844f5fac29d4c7a0a97969e551239/pyproject.toml
- https://github.com/Appsilon/TealFlowMCP/blob/1e2b752559c844f5fac29d4c7a0a97969e551239/tealflow_mcp/server.py
- https://github.com/Appsilon/TealFlowMCP/blob/1e2b752559c844f5fac29d4c7a0a97969e551239/tealflow_mcp/tools/discovery.py
- https://pypi.org/project/tealflow-mcp/0.2.0/

公式パッケージはAGPL-3.0-onlyです。このフォルダは公式コードを転載せず、公式パッケージを独立したstdioプロセスとして起動します。
