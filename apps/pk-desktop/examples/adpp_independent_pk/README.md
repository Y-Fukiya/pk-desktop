# 独立したPython計算による人工例の照合

`reference.py` は Python 標準ライブラリのみで濃度データから値を計算します。PK Desktop・R・PKNCA・admiralは呼び出しません。実装とは異なるプログラムですが、例題と参照コードは同一の作業者が用意しており、別担当者による独立検証ではありません。

## 例の条件

- 3被験者、親化合物と代謝物、経口100 mg、濃度mg/L。代謝物にも同じ親投与量を使う仮の算術例であり、代謝物のCLFO・VZFOを生理学的クリアランスや分布容積と解釈する根拠にはなりません。
- 被験者001・002は0、1、2、4、6、8時間の6点。被験者003は0、1、2時間の3点。
- AUCLSTは上昇時・ゼロを含む区間は線形、正値同士の下降時は対数台形。
- 終末相の評価範囲を手動で2〜8時間に固定し、Cmax後の正値が3点以上ある場合に、log濃度の最小二乗直線の傾きの符号を反転してλzを計算します。003は点数不足でλz以降を計算できません。
- AUCIFO = AUCLST + Clast/λz、AUCPEO = 100×(AUCIFO−AUCLST)/AUCIFO、CMAXD = CMAX/100、AUCIFOD = AUCIFO/100、CLFO = 100/AUCIFO、VZFO = CLFO/λz。
- 対象：CMAX、TMAX、AUCLST、LAMZ、LAMZHL、AUCIFO、AUCPEO、CMAXD、AUCIFOD、CLFO、VZFO。6プロファイル×11項目＝66行、数値52個、算出不可14個。

## 再実行

source直下で実行します。最初のコマンドはPythonによる計算と同梱の基準表を照合します。2つ目はNCAを実行し、基準とADPP候補の行、数値、欠測、単位、状態、PARAMCDを照合します。出力先は新しいディレクトリを指定します。

```sh
python3 apps/pk-desktop/examples/adpp_independent_pk/reference.py --check apps/pk-desktop/examples/adpp_independent_pk/expected.csv
Rscript --vanilla apps/pk-desktop/examples/adpp_independent_pk/compare.R /path/to/new/output
```

数値の絶対・相対許容差は1e-10です。差があれば `differences.csv` に記録し、非ゼロ終了します。`comparison.json` に件数、最大相対差、入力と基準のSHA-256を記録します。

自動的な終末相選択、BLQ、反復投与、投与経路の誤指定、体格補正、実データへの一般化はこの例で検証していません。ADaM仕様と用語への適合、admiralとの比較も対象外です。
