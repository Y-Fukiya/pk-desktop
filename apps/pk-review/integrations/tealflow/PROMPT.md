このフォルダの `handoff.json` を基に、TealFlowMCPでPKの探索用tealアプリを作成してください。

1. `tealflow_agent_guidance` を呼び、`tealflow_list_modules(package="general", response_format="json")` で候補を調べてください。
2. `handoff.json` の列定義と役割を確認してください。ADNCAは濃度、ADPPは計算済みPKパラメータです。ADSL等への改名や存在しない列の補完はしません。患者レコード・被験者ID・sample valuesを会話へ転記しないでください。
3. 分布図・散布図の候補に対し `tealflow_get_module_details(module_name=..., response_format="json")` を呼び、必要に応じ `tealflow_check_dataset_requirements(module_name=..., available_datasets=["ADNCA", "ADPP"])` を使ってください。これはCDISC適合性検証ではありません。
4. `tealflow_generate_module_code(module_name=...)` と `tealflow_get_app_template(response_format="json")` でひな形を取得してください。0.2.0のparameters引数は上書きを実装していないため、戻り値の列名・データ名を確認して修正してください。ここまでのツール呼出と返却コードを開発記録に残してください。
5. このフォルダの `data.R` をsourceして `pk <- load_pk_handoff("handoff.json")` を呼び、`pk$ADNCA` と `pk$ADPP` を使ってください。保存済み設定の対象個体、MDV/BLQ、期間、ANL01FL、PARAMCDは既存PKコードが適用します。単位が違う行やPARAMCDを混ぜた図・集計を作らないでください。ADPPの再計算はしません。TealFlowが提案する通常のread.csvへの置換はしません。
6. 新しいapp.Rをこの探索用フォルダだけに保存してください。既存PK Reviewや入力CSV、既存帳票、元のR環境を変更しないでください。追加パッケージを必要とする場合は、この探索用フォルダを絶対パスで指定して独立したrenv環境を準備してください。
7. 探索画面には「Exploratory — canonical reports use PK Review」と表示してください。追加したtealフィルターや図の選択は元のview.jsonへ自動反映されません。元の設定が変わったらhandoffを作り直してください。
8. 帳票に反映する条件はPK専用MCPで `pk_read_view` → `pk_update_view`（actor・reason・expected_revision必須）→ `pk_preview` → `pk_export_report` の順に適用してください。生成成功をレビュー承認として扱わないでください。

Shiny起動確認はユーザーの実行環境で行ってください。ネットワークやサーバー起動の制限を別の実行経路で迂回しないでください。
