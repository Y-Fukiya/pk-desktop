# 自動終末相選択を含む人工例の比較

同梱の `reference.py` は Python 標準ライブラリのみで計算し、R・PKNCA・PK Desktop・admiralを呼び出しません。独立した計算実装ですが、例題と基準コードは同一作業者が作成しています。別担当者による独立検証やCDISC適合性の判定ではありません。

3被験者×親化合物・代謝物、経口100 mgの架空データで、各解析単位につきCMAX後の正の実測値の末尾から3点以上の候補を作り、log濃度の最小二乗と調整R²を求めます。調整R²が最大値から0.0001未満の範囲にある候補のうち、点数の多い候補を選びます。全プロファイルは単回投与で注入終了時刻は0、BLQや欠測はありません。

- 被験者001：末尾4点（6〜12時間）が選ばれます。それより多い点は調整R²が明確に低くなります。
- 被験者002：末尾5点の調整R²が最大ですが、6点でも最大値との差が0.0001未満のため、6点（2〜12時間）が選ばれます。
- 被験者003：Cmax後は2点のため、終末相とAUC∞等は算出不可です。

CMAX・TMAX・AUCLST・LAMZ・LAMZHL・AUCIFO・AUCPEO・CMAXD・AUCIFOD・CLFO・VZFOの66行（数値52、欠測14）と、終末相の開始・終了・点数をADPP候補とNCAの記録から比較します。代謝物に同じ親投与量を使う仮の算術例であり、代謝物のCLFO・VZFOの生理学的解釈は保証しません。

source直下で実行します。2行目の出力先は存在しないディレクトリにしてください。

```sh
python3 apps/pk-desktop/examples/adpp_auto_terminal/reference.py --check apps/pk-desktop/examples/adpp_auto_terminal/expected.csv
Rscript --vanilla apps/pk-desktop/examples/adpp_auto_terminal/compare.R /path/to/new/output
```

`comparison.json` に行数・最大相対差・選択した範囲・ハッシュを、`differences.csv` に差分を記録します。絶対・相対の許容差は1e-10です。この限定された人工例では、BLQ、注入中の採血、非単調の複雑な経過、複数回投与、QC閾値の境界や大規模な一般化は未検証です。
