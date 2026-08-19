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

- 2026-08-20 07:14 更新．
- `R/` のバグ修正・リファクタリングとテスト整備 (テスト90件，すべてパス)．
- `veg_sci/appendix.R` から改ページ文字 (Form Feed, 0x0C) を除去し，空行に統一．
  他の R ファイル (`R/*.R`) と `*.Rmd` は改ページ文字を含まないため，それに合わせた．
  コードの実質的な変更はなし (制御文字のみの差分18箇所)．
- `.claude/CLAUDE.md` を git 管理下に追加．

### コミット履歴

- `3e04278` Add comprehensive appendices for R environment setup, package installation,
  data manipulation, and analysis examples

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
  新しくファイルを `R/` に足したら，このヘルパーの `app_files` にも追記する．

## 既知の課題 (未対応)

- `R/global.R` の `devtools::install_github("matutosi/ecan", force = TRUE)` は
  条件で囲まれておらず，**アプリ起動のたびに ecan を再インストールする**．
  shinyapps.io で最新の ecan を取りに行く意図があるかもしれないため，判断待ちで未変更．
- 同じく `R/global.R` の `source("cluster.R")` 等は相対パス依存で，
  カレントディレクトリが `R/` のときしか動かない．
