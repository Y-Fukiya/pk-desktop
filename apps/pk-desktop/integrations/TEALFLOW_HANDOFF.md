# TealFlowMCP → 起動中のPK Desktop

TealFlowMCP公式: https://github.com/Appsilon/TealFlowMCP
Datamods: https://dreamrs.github.io/datamods/reference/filter-data.html
teal.transform: https://insightsengineering.github.io/teal.transform/latest-tag/articles/data-merge.html

AIクライアントにTealFlowMCPとpkappの両方が接続されている場合の手順です。

1. `pk_get_status` と `pk_list_modules` で対象プロジェクト、解析revision、モジュール一覧revisionと対応範囲を読む。
2. 必要に応じて上流 `tealflow_list_modules` / `tealflow_get_module_details` でモジュールを調べる。
3. `pk_describe_data` / `pk_list_tables` で実際の列名・型と入力表を確認する。未確認の列名やPARAMCDを推測しない。
4. PK Desktopのcatalog内にある識別子と対応optionsへ変換して `pk_add_module` を呼ぶ。追加結果のID・入力行数を確認する。例えば散布図は `pk_scatterplot` と `{"x":"TIME_H","y":"AVAL","color":"SEX"}`。
5. 起動中の「探索モジュール」へ反映される。変更・追加が競合したら状態を再読込し、利用者の意図を再確認する。盲目的なrevision更新で再試行しない。
6. 未対応の上流モジュールは即時追加できない。上流生成コードを開発用として扱い、専用アダプター・入力検証・回帰試験を実装してから配布版へ加える。任意RテキストやシェルコマンドをMCPへ渡さない。

## 保存表を解析に渡す

1. `pk_import_table` または `pk_prepare_data` で入力を保存する。必要なら `pk_join_data` で結合する。
2. `pk_validate_analysis_tables(adnca, adpp, mapping, time_unit, base_revision, project_id)` へ保存表IDを渡す。
3. 検証結果と、適用で解析入力・表示条件が変わることを利用者に説明する。利用者の指示が解析への適用を含む場合は、その範囲で続行する。
4. 返された `validation_id` を `pk_apply_analysis_tables` へ渡す。検証後に解析revisionや入力ファイルが変わっていれば拒否される。
5. 新しいrevisionで `pk_update_spec` / `pk_run` を続ける。保存した探索モジュールは追加時の入力を保持するため、新データを表示したければ改めて追加する。

## 条件の例

`pk_prepare_data` は従来のfilter/select/rename/convertに加え、sort/distinct/whereを受け付けます。

```json
[{"op":"where","predicate":{"op":"|","args":[{"op":"%in%","args":[{"column":"SEX"},{"value":["F"]}]},{"op":"is.na","args":[{"column":"SEX"}]}]}}]
```

これはSEXがFまたは欠測の行を残します。式のノードは列・値・対応演算子だけで構成し、外部Rコードの評価は行いません。
