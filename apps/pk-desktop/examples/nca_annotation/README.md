# 任意のDM・VS背景情報

人工例題です。2被験者・2投与期のNCA結果へ、DMの性別・年齢とVSの身長・体重・BMI・体表面積を追加します。

1. `nca_input.csv`を読み込み、ID=ID、追加グループ=STUDYIDとPERIOD、時間=TIME、濃度=CONC、濃度単位=ng/mL、C0=補完しない、AUC=線形台形でNCAを実行・保存します。
2. 「背景情報（任意）」で保存したNCA結果を選びます。DM.sas7bdatとVS.sas7bdatを読み込みます。CSV版も同じ内容です。
3. NCAのIDをUSUBJID、STUDYIDをSTUDYIDに対応付けます。DMからSEXとAGEを選びます。
4. VSは「被験者単位・全投与期で共通」、VSTESTCD・VSSTRESN・VSSTRESU・VSDTC・VISITを選びます。HEIGHT・WEIGHT・BMI・BSAを追加します。
5. 採用条件を「列の値が一致する行」、列をVISIT、値をBASELINEにします。FOLLOWUPを混ぜると候補が重複するため停止します。
6. プレビューでは001のBMI=19.5 kg/m2、002のBMI=24.7 kg/m2を確認します。PK結果の12行と全PK値は元のままです。保存すると別の作業表になります。

記録済みのBMI・体表面積を使う例です。算出・単位換算・PK値の体格補正はしません。DM・VSを使わなくてもNCAは完了します。投与期別のVSを使う場合は「投与期など追加キー別」を選び、PERIOD等を照合キーに含めます。

採用した元行・単位・日時・来院、未照合・欠測は保存記録に残ります。保存したNCAのSTUDYIDはDM・VSのSTUDYIDと照合します。IDの先頭ゼロは保持し、数値IDへの変換は拒否します。NCAが文字化した数値の追加グループは、VSの数値をNCAと同じ文字表現にして照合します。文字列の01と数値1は別です。

`reproduce.R`をソースルートから実行すると同じ条件をR APIで再現します。実通信は専用の空プロジェクトで `tests/live_nca020_mcp.py --project PROJECT --output RESULT.json` を実行します。
