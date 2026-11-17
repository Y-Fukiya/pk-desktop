# 追加投与がある1期だけを除外する

隣の`nca_crossover`の2例×2期の人工データを使います。001の第1期に、基準投与の12時間後のEX記録RESCUEを追加します。

ソースルートから実行します。

```sh
Rscript --vanilla apps/pk-desktop/examples/nca_intervening_dose/reproduce.R
```

001の第1期は7パラメータとも`intervening_dose`で欠測です。他の3期の21パラメータは、追加前の手計算による参照値と一致します。追加投与はEXの5行目、該当する採血は12・24・48 hです。採血と投与が同時刻の場合、順序を推測しません。

投与前QCの超過とは別の処理です。投与前QCで終末相が保留された期ではCmax・Tmax・AUClastを残します。
