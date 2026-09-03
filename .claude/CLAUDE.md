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

- **`R/diversity.R` と `R/ind_val.R` の `renderReactable`・`reactableOutput` にも
  名前空間を付ける**．いまは付いていない．アプリは `R/global.R` が reactable を
  attach するので動くが，`testServer()` で module を組んだ時点で落ちるため，
  この2つのパネルはモジュールのテストが書けない
  (2026-09-04 に cluster.R で実際に踏んだ)．`reactable::` を付けるだけ．
- **【判断待ち】ordination パネルからも TWINSPAN の群を使えるようにするか**．
  Cluster パネルでは選べるようになったが，ordination は自分でクラスタを作らないので，
  パネルをまたいで群を受け渡す仕組みが要る (いまは各パネルが独立している)．

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
- `shinycssloaders` が未導入のため，アプリを起動しての UI 動作確認はできていない
  (2026-09-04，x280-home で確認．`reactable` は導入済みになっていた)．
  `R/global.R` は起動時に自分で入れるので，アプリを走らせれば解消する．
