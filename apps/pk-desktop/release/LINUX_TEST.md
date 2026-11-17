# Linux での復旧テスト

この配布作成時には Linux の実機試験は実施していません。Linux で依存関係を準備し、展開先の `source` ディレクトリから次を実行してください。

```sh
bash apps/pk-desktop/release/linux_recovery_check.sh
```

0.41 までのレビュー試験に加え、実際に使い捨てプロジェクトの所有プロセスを強制終了し、同じ場所で再起動します。終了コード 0 が成功です。利用者のプロジェクトは使用しません。

全同梱試験は Python と MCP の依存関係を用意して、次を実行します。結果ディレクトリは存在しないパスを指定します。

```sh
python3 apps/pk-desktop/release/verify_release.py --output-dir /tmp/pkdesktop-042-linux-check
```

`.github/workflows/pkdesktop-linux.yml` は GitHub Actions で R 4.6.1、ps 1.7.6／1.9.1 のマトリックスを実行する設定です。`source` をリポジトリのルートとして配置した場合に動作する想定です。依存関係をネットワークからインストールします。ワークフロー自体の実行は未確認です。
