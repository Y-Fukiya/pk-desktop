# Linux での復旧テスト

Linux で依存関係を準備し、リポジトリのルートから次を実行してください。

```sh
bash apps/pk-desktop/release/linux_recovery_check.sh
```

0.41 までのレビュー試験に加え、実際に使い捨てプロジェクトの所有プロセスを強制終了し、同じ場所で再起動します。終了コード 0 が成功です。利用者のプロジェクトは使用しません。

全同梱試験は Python と MCP の依存関係を用意して、次を実行します。結果ディレクトリは存在しないパスを指定します。

```sh
python3 apps/pk-desktop/release/verify_release.py --output-dir /tmp/pkdesktop-042-linux-check
```

GitHub ActionsではUbuntu 24.04とR 4.6.1を使い、ps 1.7.6とps 1.9.1のそれぞれで復旧試験と同梱試験を実行します。依存関係はネットワークからインストールします。[ワークフロー](../../../.github/workflows/pkdesktop-linux.yml) · [検証結果](../../../RELEASE_NOTES.md#github-actionsの検証結果)
