# 2期クロスオーバーの人工例題

2被験者にT→R、R→Tの順で各1回投与します。期の間隔は168 h、採血は−0.25、0、1、2、4、8、12、24、48 hです。DM・VSは不要です。すべて人工データです。

濃度は時刻1 hで8×scale、それ以後は8×scale×exp(−0.1×(t−1))です。0から1 hを線形台形、その後を対数台形で積分します。AUClastはscale×[4＋80×(1−exp(−4.7))]、Clast/λzはscale×80×exp(−4.7)、AUC∞は84×scaleです。λzは0.1、半減期はlog(2)/0.1です。数値の期待値はexpected.jsonにあります。

sourceディレクトリから実行します。

```sh
Rscript --vanilla apps/pk-desktop/examples/nca_crossover/reproduce.R
```

画面で試す場合はconc.csv、PC.csv、EX.csvを使います。列対応と条件はassembly.json、NCA条件はnca.jsonで確認できます。基準投与は「被験者・投与期ごとに最初」を選びます。BLQの投与前と先頭は0、途中・末尾・全点BLQは除外、LLOQは列LLOQです。NCAでは投与状況に独立した単回投与期を選び、ウォッシュアウト確認と5%・1 hの投与前QCを指定します。

基準ID001・第1期の期待値はCMAX8、TMAX1、AUCLST83.2723778319、LAMZHL6.9314718056、AUCIFO84です。washout_attestationをunconfirmedにすると前回投与のある第2期の終末相・AUC∞だけが理由付きで欠測になります。
