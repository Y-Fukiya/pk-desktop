# 手計算の人工試験仕様によるADPP候補との照合

これは `admiral` の出力ではなく、同一作業者が作成した人工例です。規制提出に使えるADaMデータの認定や、独立した担当者による検証を意味しません。

## 事前に定めた条件

- 単回投与1期、3被験者、親化合物と代謝物、検体PLASMA。入力は `concentrations.csv`。
- CMAXは観測最大値、TMAXはその時刻、AUCLSTは実測値の線形台形則。単位は濃度 ng/mL、時間 h、AUC h*ng/mL。
- 被験者001・002は4点。親化合物のAUCLSTはそれぞれ16と11.5、代謝物は8と5.75。
- 被験者003は1点のみ。CMAXとTMAXは得られるが、AUCLSTは計算不可。プロファイルはpartial、警告あり、AUCLSTに除外理由あり。
- 参照する試験固有の表は `reference_adpp.csv`。親化合物は元コード、代謝物は `M` の接尾辞。ラベルはこの架空仕様における文言であり、CDISC標準用語の主張ではない。
- 参照表の `APERIOD`・`TRTA`・`PARCAT1`・`PPSPEC` と候補の `PERIOD`・`TRT`・`ANALYTE`・`SPECIMEN` は、今回の照合用にのみ明示的に対応付ける。どの変数が標準に適合するかの判定ではない。`EXCL_PRESENT` と `WARN_PRESENT` も照合用であり、標準変数ではない。

## 再実行

source直下で次を実行します。出力先は存在しないディレクトリを指定します。指定しない場合は一時ディレクトリを使います。

```sh
Rscript --vanilla apps/pk-desktop/examples/adpp_hand_reference/compare.R /path/to/new/output
```

対応キーの重複・欠落、数値・単位・期・処置・状態・警告/除外理由の有無を検査します。`comparison.json` は行数、変数名の過不足、明示した対応関係、PARAMのラベル差、入力ファイルのSHA-256を示します。`value_differences.csv` に数値等の差、`parameter_label_differences.csv` にラベル差を残します。ラベルは仕様ごとに違うため、自動で合否をつけずレビューが必要です。数値等に差がある場合は非ゼロ終了します。

この例には反復投与、BLQ、濃度単位換算、尿中排泄、投与量由来指標、外部ADSL結合、CDISCルール検証を含みません。`admiral` を使った別担当による試験仕様準拠のADPPとの照合、CDISC適合性の検証、対象試験ごとの受入試験は別途必要です。
