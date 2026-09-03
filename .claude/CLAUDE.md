# ecanvis プロジェクト

## check の生成物の後始末

- **`R CMD check` などで作られる `*.tar.gz` は，役割が終わったら削除する**．
  結果を確認し終えたら (CRAN へ出す場合は提出が済んだら) 消してよい．
  DESCRIPTION とソースから何度でも作り直せるため，残しておく理由がない．
- 同じ理由で，`*.Rcheck/` (check の作業ディレクトリ) も確認が済んだら消す．
- 補足: `*.tar.gz` を作るのは `R CMD build` / `devtools::build()` で，
  `devtools::check()` は既定で一時ディレクトリに作るためプロジェクト直下には残らない．
  プロジェクト直下に残るのは `R CMD build` を直接実行したときが多い．
  どちらの経路でできたものでも，見つけたら消す．

## 進捗状況

### 現在の状態

- 2026-09-04 05:31 更新 (このセッション，x280-home)．
  **ordination パネルから TWINSPAN の群を使えるようにした** (ユーザ指示．テスト255件)．
  - **パネルをまたぐ受け渡しはしない**方針にした．cluster パネルが4つあるので
    「どのパネルの群か」を選ばせる UI が要り，各パネルが独立している今の作りが崩れる．
    代わりに **ordination パネル自身が TWINSPAN を回す**．
    `compute_cluster()`・`add_tw_group()` はそのまま使い回せた．
  - Show group を入れると「Add TWINSPAN group」が出て，入れると設定
    (切り値・modified・群の数) が出る二段の `conditionalPanel`．
  - **Use species scores のときは表を転置してから回す** (cluster パネルの
    「Cluster with item」と同じ)．こうしないと群が種に対応しない．
  - 選択肢の更新は cluster パネルと同じく `observeEvent` に分けた．
  - `compute_cluster()` の `d_method` を既定 `NULL` にした．
    TWINSPAN は距離を使わないので，呼ぶ側が選ばずに済む．
  - **`R/diversity.R`・`R/ind_val.R` の reactable の名前空間は `51eedcf` で適用済み**
    だった (指示を受けて確認した．`R/` に名前空間なしの呼び出しは残っていない)．

- 2026-09-04 05:02 更新 (このセッション，x280-home)．
  **テストの重複を整理し，`ind_val` の穴を埋めた** (`test_that` 68件 -> 69件，主張 225 -> 237)．
  - **完全な重複1件**: `test-utils.R` の `has_valid_cols` の同じ行が2回あった．
  - **実質的な重複4件を統合**．とくに `test-cluster.R` の `indiv()` の2件は，
    2026-09-04 の組み替えで `indiv()` が `cls_show_group` を**本体で使わなくなった**ため，
    同じ経路を2度通っていた (ordination 側は今も分岐と副作用を持つので残した)．
    ほかは `cut_conti` の2件・メッセージ関数の2件・`test-diversity.R` の `all_data` の行・
    `test-load_data.R` の `has_valid_cols` だけのテスト．
  - **主張が弱い検査を1件修正**: `expect_error(two_way())` は実際には `req()` の
    `shiny.silent.error` なので，**本物のバグでも通ってしまう**状態だった．
    `class = "shiny.silent.error"` を付けた (`test-no_group.R` に合わせた)．
  - **`test-ind_val.R` を新設** (5件)．整理の裏で見つかった穴で，ISA パネルは
    「群が無いとき」しか検査されていなかった．結果の形と丸め・`filter_ind_val` を
    通した描画・絞り込みで空になる場合・表とダウンロードのファイル名・
    量が数値でないときの注意書きを見る．
  - **残すと決めたもの**: `test-no_group.R` の前提の確認 (ecan 側が変わると
    残り4件が何も検査しなくなる)，unit とモジュールで層が違う検査．

- 2026-09-04 04:28 更新 (このセッション，x280-home)．
  **全体を点検してバグを直し，リファクタリングとテスト追加をした** (テスト95件 -> 225件)．
  - **直したバグ4件**
    1. **ordination の軸番号**．`pcoa` は成分が2つしか返らないのに軸は4まで選べ，
       `names()[4]` が `NA` になって落ちていた．さらに軸3は `ord_extract_score()` が
       付ける**文字列の行名列**を拾っていた．`score_axes()`・`pick_axis()` で
       **得点列だけ**から選び，範囲を外れたら丸めて `msg_axis_clamped()` で知らせる．
    2. **群の列が無いデータで ISA が赤いエラー**になっていた
       (`ecan::ind_val()` が `Needs "group" input` で停止)．`has_group()` で判定し，
       `msg_no_group()` を出す．cluster と ordination は**群なしの図に落とす**
       (以前は空白のままだった)．
    3. **単位と項目に同じ列を選ぶと，列が入れ替わった表を下流に返していた**
       (`relocate()` が重複を落とすため `stand, cover, species` になる)．
       警告は出ていたが表は流れていた．`req()` で止め，直前の表を保つ．
    4. **`reactable::` の付け忘れ** (diversity・ind_val・load_data)．
       アプリは動くが，`testServer()` で組んだ時点で落ちるためテストが書けなかった．
  - **リファクタリング**: `round_numeric()`・`has_group()` に共通処理を集約．
    論理入力の `if(input$x)` を `isTRUE()` にした (未設定時に落ちる)．
    使われていない `dots2list()` を削除．
  - **テスト**: `test-ui.R` (UI が組めるか・名前空間・fspa 不在) と
    `test-no_group.R` (群が無いときの4パネル) を新設．軸まわりの回帰も追加．
  - この PC に `shinycssloaders` を入れた．これで UI 関数もテストできる．

- 2026-09-04 03:41 更新 (このセッション，x280-home)．
  **TWINSPAN の群を Show group に出し，二元表を Cluster パネルに載せた** (ユーザ指示)．
  - `add_tw_group()` で TWINSPAN の群を data_in に1列足し，Show group の
    選択肢に出す (`cols_one2multi()` が拾う)．列名の衝突は `unique_col_name()` で避ける．
    `$classification` は**表を転置しても列名が `stand` のまま**なので，
    Cluster with item のときはそこに種名が入る．`indiv` と突き合わせれば両方に効く．
  - 二元表は `tw_two_way_df()` で reactable 用の data.frame にする．
    行の path を列にし，**標本の二分の桁を下の行に置く** (原典の印刷出力と同じ並び)．
    cells は level / abundance を選べ，tsv で download もできる．
  - サーバを組み替えた: クラスタ結果を `cls_raw()` に切り出し，
    選択肢の更新は `indiv()` の副作用をやめて `observeEvent` に分けた．
  - **`reactable::renderReactable` と `reactable::reactableOutput` は名前空間付きで呼ぶ**．
    テストは reactable を attach しないので，付けないと `output$` への代入の時点で落ちる．
  - テスト157件すべてパス (追加前128件)．

- 2026-09-04 03:12 更新 (このセッション，x280-home)．
  **Cluster パネルに TWINSPAN を追加した** (ecan 0.2.2.9000 の `twinspan()`)．
  - cluster method の選択肢に `twinspan` を足した．
  - TWINSPAN は距離を使わないので，**選ぶと distance method を伏せる**
    (`conditionalPanel`)．代わりに TWINSPAN の設定を出す:
    pseudospecies の切り値・modified TWINSPAN・群の数 (0 は制限なし)．
  - 分岐は `R/utils.R` の `compute_cluster()` に置いた．
    `ecan::cluster()` と同じく `$clustering_method` を持たせ，
    距離を使わないことを示すため `$distance_method` は `NULL` にする．
    `stats::as.hclust()` を通すので `cls_color()`・`cls_add_group()` も従来どおり効く．
  - 入力の解釈は `parse_cut_levels()`・`as_n_clusters()` に分けた．
    入力途中の文字列でアプリが止まらないようにしてある．
  - テストを `tests/testthat/test-twinspan.R` に追加．**128件すべてパス** (旧95件)．

- 2026-09-04 02:56 更新 (このセッション，x280-home)．
  **ecan 0.2.2.9000 (GitHub 版) の更新を反映した**．
  - 引数名 `inculde_self` を `include_self` に直した (4ファイル)．旧綴りも当面は通るが，
    ecan 側が正しい綴りへ移ったので追随する．
  - ordination の選択肢から **`"fspa"` を外した**．ecan 0.2.1 で削除済みで
    (パッケージ dave が CRAN からアーカイブされたため)，選ぶとエラーになっていた．
  - 同じ理由で **`dave` をインストール対象から外した** (`R/global.R`・README)．
  - README の `install_github(..., force = TRUE)` を `force` なしにした
    (`R/global.R` は 2026-08-20 に対処済みで，README だけ残っていた)．
  - この PC の ecan を 0.2.1 → 0.2.2.9000 に更新．テスト95件すべてパス．
    pcoa が `$st_scores` に標本の座標を返すようになった修正も，これで効くようになった．

- 2026-08-20 07:23 更新．
- `R/global.R` を整理 (ecan の毎回再インストールを解消，相対パス依存を緩和)．
- `R/` のバグ修正・リファクタリングとテスト整備 (テスト95件，すべてパス)．
- `veg_sci/appendix.R` から改ページ文字 (Form Feed, 0x0C) を除去し，空行に統一．
  他の R ファイル (`R/*.R`) と `*.Rmd` は改ページ文字を含まないため，それに合わせた．
  コードの実質的な変更はなし (制御文字のみの差分18箇所)．
- `.claude/CLAUDE.md` を git 管理下に追加．

### コミット履歴

- `3e04278` Add comprehensive appendices for R environment setup, package installation,
  data manipulation, and analysis examples

### 次にやること

- **【判断待ち】cluster と ordination で同じ TWINSPAN の群を使いたくなったら，
  パネルをまたぐ受け渡しを考える**．2026-09-04 は各パネルが自分で回す形にした
  (理由は上の記録)．同じ設定を入れれば同じ群になるので，当面は困らないはず．
  困るとすれば，同じ計算を2度することと，設定を2箇所に入れる手間．

## テスト

- `tests/testthat/` に testthat (3rd edition) のテストを置く．実行は次のいずれか．

  ```
  Rscript tests/testthat.R
  Rscript -e 'testthat::test_dir("tests/testthat")'
  ```

- **`devtools::test()` や `devtools::load_all()` は使わない**．
  `R/global.R` は読み込むだけでパッケージのインストールが走り，
  `R/ui.R` と `R/server.R` はトップレベル式なので，パッケージとして読み込めない．
  代わりに `tests/testthat/helper-ecanvis.R` が関数定義だけのファイルを `source()` する．
  新しくファイルを `R/` に足したら，`R/global.R` とこのヘルパーの
  両方の `app_files` に追記する (ずれると `test-global.R` が落ちる)．

## 既知の課題

- `R/global.R` を実行するとパッケージのインストールが走るため，
  テストからは読み込まない．`source()` するファイルの一覧がずれないよう，
  `tests/testthat/test-global.R` が `global.R` とヘルパーの `app_files` を突き合わせる．
- パッケージとしての体裁は未整備 (NAMESPACE と man/ が無く，
  DESCRIPTION の Imports も実態と合っていない)．
  shiny アプリとして shinyapps.io へ配置する運用のため，当面は問題にならない．
- **ブラウザでアプリを起動しての目視確認はしていない**．
  `reactable`・`shinycssloaders` は 2026-09-04 に x280-home へ導入したので，
  UI 関数が組めることは `test-ui.R` で機械的に確かめられる．
  ただし `conditionalPanel` の表示切り替えや図の見た目は，実際に走らせないと分からない．
