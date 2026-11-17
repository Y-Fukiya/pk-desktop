# PK Desktop 0.61.1-alpha

PK Desktopは、濃度データの前処理、非コンパートメント解析（NCA）、結果の可視化と保存をローカルで試せるRアプリです。合成データを使った探索・QC向けのアルファ版です。日本語と英語の表示を切り替えられます。英語表示はプレビューです。

## ソースからNCAを試す

R 4.4以降を用意してください。検証環境はR 4.6.1とPKNCA 0.12.1です。リポジトリのルートで次を実行すると、必要なRパッケージをセットアップし、既定のブラウザーでNCA画面を開きます。

```sh
Rscript --vanilla apps/pk-desktop/install.R
Rscript --vanilla apps/pk-desktop/launch.R --mode nca
```

練習用には、単回経口投与の合成CSV `apps/pk-desktop/examples/nca_dose_body/oral_absorption.csv` を選べます。この例の入力単位は時間（h）と濃度（mg/L）です。列の対応、投与状況、BLQの扱い、C0の方針、単位を確認してNCAを実行します。起動したターミナルで `Ctrl+C` を押すと終了します。[初回利用ガイド](FIRST_USE_GUIDE.md)

## ビルドと検証

- R環境の準備、ブラウザー版の起動、TauriによるmacOSネイティブ版のビルドを確認できます。[ビルド・起動手順](BUILD.md)
- 今回のCI修正と、GitHub Actionsでの検証結果を確認できます。[リリースノート](RELEASE_NOTES.md)

## R同梱のmacOS版

別配布の0.61.0-alphaのR同梱macOS版は、Apple Silicon向けの**アドホック署名版**です。Developer ID署名・Apple公証は未実施です。同梱Rによる起動とNCAは開発時のMacで確認しました。**別のMacでのダウンロード後の起動とGatekeeper通過は未検証です。** ダウンロードしたアプリの起動がGatekeeperでブロックされる場合があります。[AppleのGatekeeperの説明](https://support.apple.com/en-us/102445)

## ライセンス

[MIT License](LICENSE)

