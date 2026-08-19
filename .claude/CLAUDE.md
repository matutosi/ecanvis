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
- `reactable` と `shinycssloaders` が開発機に未導入のため，
  アプリを起動しての UI 動作確認はできていない．
