# PK Desktop 0.61.1-alpha リリースノート

LinuxのGitHub Actionsで試験を実行できるようにしたソースのプレリリースです。NCAの計算ロジックは0.61.0-alphaと同じで、PKNCA 0.12.1を使います。画面機能と前版の実機検証については、0.61.0-alphaのリリースノートに記載しています。[0.61.0-alphaのリリースノート](https://github.com/Y-Fukiya/pk-desktop/releases/tag/v0.61.0-alpha)

## CIの修正

- UbuntuにTIFF、JPEG、WebPの開発ライブラリを追加し、`ragg` をビルドできるようにしました。
- R 4.6.1で旧版のps 1.7.6を読み込めるように、ps 1.7.6のビルド時だけRの互換オプションを有効にしました。
- インストーラー試験がプロジェクト内のRライブラリから `testthat` を読み込むようにしました。

アプリの起動画面とバージョン情報を `0.61.1-alpha` にそろえました。

## GitHub Actionsの検証結果

CI修正コミット `609ce22` をUbuntu 24.04、R 4.6.1、PKNCA 0.12.1で検証しました。ps 1.7.6とps 1.9.1は、いずれもLinuxの復旧・投与期順序の試験と同梱試験120件に合格しました。合成データを使ったChromiumの画面試験も5件すべて通りました。[CI修正の実行結果](https://github.com/Y-Fukiya/pk-desktop/actions/runs/44037733787)

## 配布物

このプレリリースでは、GitHubが生成するソースアーカイブを公開します。[ビルド・起動手順](BUILD.md)

別配布の0.61.0-alphaのR同梱macOS版は、Apple Silicon向けのアドホック署名版です。Developer ID署名とApple公証は未実施です。R未導入の別のMacでのGatekeeper通過、初回起動、NCA実行、結果表示は未検証です。

