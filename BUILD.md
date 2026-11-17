# PK Desktopのビルド・起動手順

コマンドはリポジトリのルートから実行してください。ネイティブ版のビルドは手順内で作業ディレクトリを指定します。

## R環境を準備する

R 4.4以降を使います。検証環境はR 4.6.1とPKNCA 0.12.1です。セットアップは不足パッケージをプロジェクト内の `.runtime/R-library` にインストールし、PKNCAを0.12.1に固定します。[RのmacOS向け配布ページ](https://cran.r-project.org/bin/macosx/)

```sh
Rscript --version
Rscript --vanilla apps/pk-desktop/install.R
Rscript --vanilla apps/pk-desktop/install.R --check
```

検証時の168パッケージのバージョンを揃える場合は、R 4.6.1で `restore.R` を使います。既存環境での読み込みとバージョン照合は確認済みです。新規環境でのネットワーク経由の完全復元は未検証です。[R環境の詳細](apps/pk-desktop/RUNTIME_ENVIRONMENT.md)

```sh
Rscript --vanilla apps/pk-desktop/restore.R
Rscript --vanilla apps/pk-desktop/restore.R --check
```

## ブラウザーで起動する

NCAだけを開くには `--mode nca` を指定します。

```sh
Rscript --vanilla apps/pk-desktop/launch.R --mode nca
```

合成デモを使う全体画面は `--mode gui` で起動できます。デモのADPPには計算済みのPKパラメータが入っています。

```sh
Rscript --vanilla apps/pk-desktop/launch.R --mode gui
```

アプリはローカルホストで起動します。既定のUIとAPIのポートはNCAが3879と3880、全体画面が3877と3878です。使用中なら `--port` と `--api-port` に空きポートを指定します。プロジェクトの保存先は `--project` で指定できます。`--no-browser` を付けるとターミナルに認証付きローカルURLが表示されます。このURLは起動したMacのブラウザーで開いてください。終了は `Ctrl+C` です。

## macOSネイティブ版をビルドする

Apple Silicon MacでXcodeの開発ツール、Rust、Tauri CLI 2系を用意します。検証に使ったTauri CLIは2.0.3です。上の手順でR環境も準備してください。[Tauriのビルド要件](https://v2.tauri.app/start/prerequisites/)

```sh
xcodebuild -version
rustc --version
cargo tauri --version
```

Tauri CLI 2.0.3を用意する場合は次を実行します。

```sh
cargo install tauri-cli --version 2.0.3 --locked
```

```sh
cd apps/pk-desktop
cargo tauri build --ci --bundles app --config '{"bundle":{"active":true}}' -- --locked
open 'src-tauri/target/release/bundle/macos/PK Desktop alpha.app'
```

上の出力パスはCargoの既定値です。`CARGO_TARGET_DIR` を指定した場合はそのディレクトリの `release/bundle/macos` を参照します。このビルドは、ビルドに使ったチェックアウトのソースとインストール済みRを使います。[TauriのmacOSアプリバンドル](https://v2.tauri.app/distribute/macos-application-bundle/)

初回画面でプロジェクト保存先を指定します。合成デモを使う場合は入力フォルダの初期値を使います。Rscriptの初期値が手元のRと異なる場合は、ターミナルで `command -v Rscript` を実行し、その絶対パスを指定します。**プロジェクトを開く**を押し、解析ウィンドウで **NCA** を選びます。

## R同梱macOS版を作る

通常のTauriビルドはインストール済みRを使います。R同梱版を作る場合は、ビルドした `.app`、Apple Silicon用のR 4.6.1フレームワーク、対応する検証済みRパッケージライブラリをステージングスクリプトに渡します。同梱試験は `nanoparquet` と `bit64` も読み込みます。アプリの本体、入力用の合成デモ、Rをステージングし、Mach-Oの依存先、同梱Rの読み込み範囲、スモーク試験をチェックします。

```sh
# リポジトリのルート
python3 apps/pk-desktop/release/stage_macos.py --help
```

`--app` にビルド出力、`--output` にソース外の新規 `.app` パス、`--framework` にRフレームワーク、`--library` にパッケージライブラリを指定します。パッケージが外部の非システムライブラリを参照している場合は、依存先を確認して `--native-file` で同梱対象ファイルを指定します。検証ビルドでは `classInt` が参照するFortranランタイムの3ファイルを追加しました。ステージングが成功したら、ローカル検証用に `codesign --force --sign -` で外側の `.app` をアドホック署名し、`codesign --verify --deep --strict` で検査します。[ステージングスクリプト](apps/pk-desktop/release/stage_macos.py)

既存の別配布R同梱版はApple Silicon向けの**アドホック署名版**です。Developer ID署名・Apple公証は未実施で、**Rが入っていない別のMacでのダウンロード、Gatekeeper通過、初回起動からNCAまでの動作は未検証**です。[AppleのGatekeeperの説明](https://support.apple.com/en-us/102445)

## 同梱試験を実行する

標準検証セットにはR、Python、Node.jsが必要です。Python環境にMCP依存パッケージを用意し、新しい出力先を指定して実行します。ブラウザー試験を含める場合はPlaywrightとブラウザーも準備し、`--with-browser` を追加します。[標準検証の詳細](apps/pk-desktop/release/VERIFY_RELEASE.md) · [ブラウザー試験の手順](BROWSER_TEST_GUIDE.md)

```sh
python3 -m venv .venv
.venv/bin/python -m pip install -r apps/pk-desktop/requirements-mcp.txt
pk_verification_dir="$(mktemp -d)"
.venv/bin/python apps/pk-desktop/release/verify_release.py \
  --rscript "$(command -v Rscript)" --output-dir "$pk_verification_dir/results"
```

