  # 松村 俊和. 2026. 植生調査資料の多変量解析の基礎とRでの実行方法. 植生情報, 30, ??-??.

  # Appendix 1：Rの実行環境
sessionInfo() # 以下の ## から右側は出力
  ## R version 4.5.1 (2025-06-13 ucrt)
  ## Platform: x86_64-w64-mingw32/x64
  ## Running under: Windows 11 x64 (build 26220)
  ## 
  ## Matrix products: default
  ##   LAPACK version 3.12.1
  ## 
  ## locale:
  ## [1] LC_COLLATE=Japanese_Japan.utf8  LC_CTYPE=Japanese_Japan.utf8   
  ## [3] LC_MONETARY=Japanese_Japan.utf8 LC_NUMERIC=C                   
  ## [5] LC_TIME=Japanese_Japan.utf8    
  ## 
  ## time zone: Etc/GMT-9
  ## tzcode source: internal
  ## 
  ## attached base packages:
  ## [1] datasets  utils     graphics  grDevices stats     methods   base
  ## 
  ## other attached packages:
  ##  [1] cowplot_1.2.0   ggsci_4.1.0     ggrepel_0.9.6   ggplot2_3.5.2  
  ##  [5] ggdendro_0.2.0  ecan_0.2.1.9000 vegan_2.7-1     permute_0.9-8  
  ##  [9] tidyverse_2.0.0 MASS_7.3-65    
  ## 
  ## loaded via a namespace (and not attached):
  ##  [1] vctrs_0.6.5        nlme_3.1-168       cli_3.6.5         
  ##  [4] rlang_1.1.6        generics_0.1.4     glue_1.8.0        
  ##  [7] scales_1.4.0       grid_4.5.1         tibble_3.3.0      
  ## [10] lifecycle_1.0.4    cluster_2.1.8.1    compiler_4.5.1    
  ## [13] dplyr_1.1.4        RColorBrewer_1.1-3 Rcpp_1.0.14       
  ## [16] pkgconfig_2.0.3    mgcv_1.9-3         farver_2.1.2      
  ## [19] lattice_0.22-7     R6_2.6.1           tidyselect_1.2.1  
  ## [22] splines_4.5.1      pillar_1.11.1      parallel_4.5.1    
  ## [25] magrittr_2.0.3     Matrix_1.7-3       withr_3.0.2       
  ## [28] gtable_0.3.6      


  # Appendix 2：パッケージのインストール
install.packages("tidyverse")  # コードを明快に
install.packages("vegan")      # 植生解析の定番
install.packages("ecan")       # 解析を簡単に
install.packages("ggdendro")   # 図示で使用
install.packages("dendextend") # 図示で使用
install.packages("ggrepel")    # 図示で使用
install.packages("ggsci")      # 図示で使用
install.packages("cowplot")    # 図示で使用

  ## (省略)


  # Appendix 3：パッケージの呼び出し
library(vegan)
library(ecan)
library(tidyverse)
library(ggdendro)
library("dendextend")
library(ggrepel)
library("ggsci")
library("cowplot")
  ## (省略)


  # Appendix 4：messy dataの例
tidyr::table2
  ## # A tibble: 12 × 4
  ##    country      year type            count
  ##    <chr>       <dbl> <chr>           <dbl>
  ##  1 Afghanistan  1999 cases             745
  ##  2 Afghanistan  1999 population   19987071
  ##  3 Afghanistan  2000 cases            2666
  ##  4 Afghanistan  2000 population   20595360
  ##  5 Brazil       1999 cases           37737
  ## (省略)


  # Appendix 5：tidy dataの例
tidyr::table2 |>
  tidyr::pivot_wider(
    id_cols = c(country, year), 
    names_from = type, 
  values_from = count)
  ##  # A tibble: 6 × 4
  ##    country      year  cases population
  ##    <chr>       <dbl>  <dbl>      <dbl>
  ##  1 Afghanistan  1999    745   19987071
  ##  2 Afghanistan  2000   2666   20595360
  ##  3 Brazil       1999  37737  172006362
  ##  4 Brazil       2000  80488  174504898
  ##  5 China        1999 212258 1272915272
  ## (省略)


  # Appendix 6：データの読み込みと保存(擬似コード)
  # CSVの読み込み
path_stand <- "C:/hoge/huga/stand.csv"
df_st <- readr::read_csv(path_stand)
  # TSVの読み込み
path_species <- "C:/hoge/huga/species.tsv"
df_sp <- readr::read_csv(path_species)
  # Excel形式の読み込み
path_data <- "C:/hoge/huga/data.xlsx"
df_data <- openxlsx::read.xlsx(path_data, sheet = 1)
  # CSVでの保存
readr::write_csv(df_st, path_stand)
  # TSVでの保存
readr::write_tsv(df_sp, path_species)
  # Excel形式での保存
openxlsx::write.xlsx(df_data, path_data)


  # Appendix 7：地点データ(dune.env)の整理
data(dune.env, package = "vegan") # 地点データ
dune_env_df <- 
  tibble::tibble(dune.env) |>            # A1, Moistureなどの情報
  tibble::rownames_to_column("stand") |> # 地点名を列に変換
  print()
  ##  # A tibble: 20 × 6
  ##     stand    A1 Moisture Management Use      Manure
  ##     <chr> <dbl> <ord>    <fct>      <ord>    <ord> 
  ##   1 1       2.8 1        SF         Haypastu 4     
  ##   2 2       3.5 1        BF         Haypastu 2     
  ##   3 3       4.3 2        SF         Haypastu 4     
  ##   4 4       4.2 2        SF         Haypastu 4     
  ##   5 5       6.3 1        HF         Hayfield 2     
  ## (省略)


  # Appendix 8：組成データ(dune)の整理
data(dune, package = "vegan") # 組成データ
tibble(dune)                  # データの表示
  ##  # A tibble: 20 × 30
  ##     Achimill Agrostol Airaprae Alopgeni Anthodor Bellpere # (省略)
  ##        <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl> # 
  ##   1        1        0        0        0        0        0 # 
  ##   2        3        0        0        2        0        3 # 
  ##   3        0        4        0        7        0        2 # 
  ##   4        0        8        0        2        0        2 # 
  ##   5        2        0        0        0        4        2 # 
  ## (省略)
dune_df <- 
  dune |>             # 組成表形式
  ecan::table2df(     # df形式に変換
    st = "stand",     # 地点を"stand"列に
    sp = "species",   # 種を"species"列に
    ab = "cover") |>  # 値を"cover"列に
  print()
  ## # A tibble: 197 × 3
  ##    stand species  cover
  ##    <chr> <chr>    <dbl>
  ##  1 1     Achimill     1
  ##  2 1     Elymrepe     4
  ##  3 1     Lolipere     7
  ##  4 1     Poaprat      4
  ##  5 1     Poatriv      2
  ## (省略)


  # Appendix 9：種データ(架空)の生成
sp_dummy <- tibble::tibble(
  "species" = colnames(dune), 
  "dummy_1" = stringr::str_sub(colnames(dune), 1, 1),    # 1文字目
  "dummy_6" = stringr::str_sub(colnames(dune), 6, 6)) |> # 6文字目
  print()
  ## # A tibble: 30 × 3
  ##    species  dummy_1 dummy_6
  ##    <chr>    <chr>   <chr>  
  ##  1 Achimill A       i      
  ##  2 Agrostol A       t      
  ##  3 Airaprae A       r      
  ##  4 Alopgeni A       e      
  ##  5 Anthodor A       d      
  ## (省略)


  # Appendix 10：データの結合
df <- 
  dune_df |>                       # 組成
  dplyr::left_join(dune_env_df) |> # 地点
  dplyr::left_join(sp_dummy)  |>   # 種
  print()
  ## Joining with `by = join_by(stand)`
  ## Joining with `by = join_by(species)`
  ## # A tibble: 197 × 10
  ##    stand species  cover    A1 Moisture Management Use      Manure dummy_1 dummy_6
  ##    <chr> <chr>    <dbl> <dbl> <ord>    <fct>      <ord>    <ord>  <chr>   <chr>  
  ##  1 1     Achimill     1   2.8 1        SF         Haypastu 4      A       i      
  ##  2 1     Elymrepe     4   2.8 1        SF         Haypastu 4      E       e      
  ##  3 1     Lolipere     7   2.8 1        SF         Haypastu 4      L       e      
  ##  4 1     Poaprat      4   2.8 1        SF         Haypastu 4      P       a      
  ##  5 1     Poatriv      2   2.8 1        SF         Haypastu 4      P       i      
  ## (省略)


  # Appendix 11：データ変換の例
bb_scale <- c("5", "4", "3", "2", "1", "+", "0")
percen_med <- c(87.5, 62.5, 37.5, 17.5, 5.5, 0.55, 0)
percentage <- c(80, 60, 30,  15,   8,  0.1, 0)
squared <- percentage^0.5
tibble::tibble(
  "階級値" = bb_scale, 
  "中央値" = percen_med, 
  "百分率" = percentage, 
  "平方根変換" = squared, 
)
  ## # A tibble: 7 × 4
  ##   階級値 中央値 百分率 平方根変換
  ##   <chr>   <dbl>  <dbl>      <dbl>
  ## 1 5       87.5    80        8.94 
  ## 2 4       62.5    60        7.75 
  ## 3 3       37.5    30        5.48 
  ## 4 2       17.5    15        3.87 
  ## 5 1        5.5     8        2.83 
  ## 6 +        0.55    0.1      0.316
  ## 7 0        0       0        0    

  # Appendix 12：相対優占度の計算とHellinger変換
val <- c(4, 3, 2, 1)
(proportion <- val / sum(val)) # 相対優占度
  ## [1] 0.4 0.3 0.2 0.1
proportion ^ 0.5 # Hellinger変換
  ## [1] 0.6324555 0.5477226 0.4472136 0.3162278


  # Appendix 13：Simpsonの指数の計算
  # 5種とも全て20のとき
val <- rep(20, 5)
nj <- sum(val)     # 出現量の合計
pi_1 <- val / nj   # 相対優占度
d_1 <- sum(pi_1 ^2)
c(d_1, 1 - d_1, 1 / d_1)
  ## [1] 0.2 0.8 5.0
 # 1種が60,他が10のとき
val <- c(10, 10, 60, 10, 10)
nj <- sum(val)     # 出現量の合計
pi_2 <- val / nj   # 相対優占度
d_2 <- sum(pi_2 ^ 2)
c(d_2, 1 - d_2, 1 / d_2)
  ## [1] 0.4 0.6 2.5


  # Appendix 14：Shannonの情報量指数の計算
-sum(pi_1 * log(pi_1))
  ## [1] 1.609438
-sum(pi_2 * log(pi_2))
  ## [1] 1.227529


  # Appendix 15：α多様性の計算
div <- 
  ecan::shdi(df, 
             stand = "stand", 
             species = "species", 
             abundance = "cover") |>
  print()
  ## # A tibble: 20 × 5
  ##    stand     s     h     d     i
  ##    <chr> <int> <dbl> <dbl> <dbl>
  ##  1 1         5  1.44 0.735  3.77
  ##  2 10       12  2.40 0.903 10.3 
  ##  3 11        9  2.11 0.867  7.53
  ##  4 12        9  2.11 0.869  7.61
  ##  5 13       10  2.10 0.852  6.76
  ## (省略)


  # Appendix 16：地点の属性別のα多様性の平均値
dune_env_df <- dplyr::left_join(dune_env_df, div) # 地点を結合
dune_env_df |>
  dplyr::group_by(Management) |>        # Managementでグループ化
  dplyr::summarise_if(is.numeric, mean) # 数値の列で平均
  ## # A tibble: 4 × 6
  ##   Management    A1     s     h     d     i
  ##   <fct>      <dbl> <dbl> <dbl> <dbl> <dbl>
  ## 1 BF          3.43 10.3   2.25 0.887  8.98
  ## 2 HF          4.26 12.6   2.46 0.908 10.9 
  ## 3 NM          6.1   8     2.00 0.854  6.92
  ## 4 SF          4.8   9.17  2.04 0.846  7.14

  # Appendix 17：グループ別でのα多様性の図示
dune_env_df |>
  ggplot2::ggplot(
    ggplot2::aes(x = Management, y = i)) +        # 1 / d
  ggplot2::geom_boxplot(outlier.shape = NA) +     # 外れ地を非表示
  ggplot2::geom_jitter(height = 0, width = 0.1) + # 重複を避けてプロット
  ggplot2::theme_bw()
  ## (図1参照)

  # Appendix 18：クラスター分析用の架空データ
set.seed(2)            # 乱数種の設定
label <- letters[1:10] # アルファベットのaからj
x <- runif(10)         # 正規分布の乱数
y <- runif(10)
df_cls <- 
  tibble::tibble(rownames = label, label = label, x = x, y = y) |>
  tibble::column_to_rownames("rownames") |> # labelを行名に
  print()
  ##   label         x          y
  ## a     a 0.1848823 0.55267407
  ## b     b 0.7023740 0.23889476
  ## c     c 0.5733263 0.76051331
  ## d     d 0.1680519 0.18082010
  ## e     e 0.9438393 0.40528218
  ## f     f 0.9434750 0.85354845
  ## g     g 0.1291590 0.97639849
  ## h     h 0.8334488 0.22582546
  ## i     i 0.4680185 0.44480923
  ## j     j 0.5499837 0.07497942


  # Appendix 19：架空データの図示
df_cls |>
  ggplot2::ggplot(ggplot2::aes(x, y, label = label)) + 
  ggplot2::geom_text(size = 10) + 
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(size = 14), 
    axis.title.y = ggplot2::element_text(size = 14),
    axis.text = ggplot2::element_text(size = 14)
  ) 
  ## (図2参照)


  # Appendix 20：架空データでの距離の算出
dist_euc <- 
  df_cls |>
  dplyr::select(x, y) |>
  stats::dist(method = "euclidean") |>
  round(3) |>
  print()
  ##       a     b     c     d     e     f     g     h     i
  ## b 0.605
  ## c 0.441 0.537
  ## d 0.372 0.537 0.707
  ## e 0.773 0.293 0.513 0.808
  ## f 0.816 0.660 0.382 1.027 0.448
  ## g 0.427 0.934 0.494 0.797 0.995 0.824
  ## h 0.726 0.132 0.595 0.667 0.211 0.637 1.029
  ## i 0.303 0.312 0.333 0.400 0.477 0.627 0.630 0.426
  ## j 0.601 0.224 0.686 0.396 0.514 0.872 0.995 0.321 0.379


  # Appendix 21：架空データでのクラスター分析と図示
hc <- stats::hclust(dist_euc, method = "single") # "single"：最近隣法
ggdendro::ggdendrogram(hc)
  ## (図3左参照)


  # Appendix 22：ユークリッド距離とマンハッタン距離の考え方
df_euc <- tibble::tibble(x = c(1,3), y = c(1,2), size = c(1,1))
df_man <- tibble::tibble(x = c(1,3,3), y = c(1,1,2), size = c(1, 0, 1))
df_euc |>
  ggplot2::ggplot(ggplot2::aes(x, y)) + 
  ggplot2::geom_point(ggplot2::aes(size = size)) + 
  ggplot2::geom_path() + 
  ggplot2::geom_path(data = df_man, linetype = "dashed") + 
  ggplot2::coord_fixed() + 
  ggplot2::theme_bw() + 
  ggplot2::theme(legend.position = "none")
  ## (図4参照)


  # Appendix 23：クラスター分析の結合方法の比較
 # 最近隣法
gg_sing <- 
  stats::hclust(dist_euc, method = "single") |>
  ggdendro::ggdendrogram()
  # 非荷重群平均法(UPGMA)
gg_aver <-
  stats::hclust(dist_euc, method = "average") |>
  ggdendro::ggdendrogram()
 # Ward法
gg_ward <-
  stats::hclust(dist_euc, method = "ward.D2") |>
  ggdendro::ggdendrogram()
  # 図示
cowplot::plot_grid(
  gg_sing, gg_aver, gg_ward,
  nrow = 1)
  ## (図3参照)


  # Appendix 24：Bray-Curtis距離と非加重平均法によるクラスター分析
cls <- 
  df |>
  ecan::df2table(st = "stand", sp = "species", ab = "cover") |> # 組成形式に
  ecan::cluster(
    c_method = "average", # 非荷重群平均方
    d_method = "bray")    # Bray-Curtis距離
cls # 結果の表示
  ## Call:
  ## stats::hclust(d = d, method = c_method)
  ## 
  ## Cluster method   : average 
  ## Distance         : bray 
  ## Number of objects: 20 


  # Appendix 25：クラスター分析結果の構造の表示
str(cls)
  ## List of 9
  ##  $ merge            : int [1:19, 1:2] -6 -5 -3 -9 -10 -14 -2 -11 -16 -8 ...
  ##  $ height           : num [1:19] 0.0833 0.1156 0.1304 0.1818 0.1904 ...
  ##  $ order            : int [1:20] 14 15 16 20 17 19 1 2 3 4 ...
  ##  $ labels           : chr [1:20] "1" "2" "3" "4" ...
  ##  $ method           : chr "average"
  ##  $ call             : language stats::hclust(d = d, method = c_method)
  ##  $ dist.method      : chr "bray"
  ##  $ clustering_method: chr "average"
  ##  $ distance_method  : chr "bray"
  ##  - attr(*, "class")= chr "hclust"


  # Appendix 26：クラスター分析結果の図示
ggdendro::ggdendrogram(cls)
  ## (省略)


  # Appendix 27：クラスター分析結果へのグループ名の追加
indiv <- "stand"
group <- "Use"
cls_group <- 
  ecan::cls_add_group(
    cls = cls, 
    df = dune_env_df, 
    indiv = indiv, 
    group = group)
ggdendro::ggdendrogram(cls_group) + 
  ggplot2::theme(text = element_text(family = "mono")) # 等幅フォント
  ## (図5参照)


  # Appendix 28：クラスター分析結果へのグループの着色
col <- 
  ecan::cls_color(
    cls = cls, 
    df = dune_env_df, 
    indiv = indiv, 
    group = group)
cls_colored <- stats::as.dendrogram(cls_group)
par(family = "mono",     # 等幅フォント
  oma = c(0.5, 0, 0, 0)) # 下余白：0.5
plot(cls_colored)
dendextend::colored_bars(
  colors = col, 
  cls_colored, 
  group, 
  y_shift = 0, 
  y_scale = 3)
par(new = TRUE)
plot(cls_colored)
  ## (省略)


  # Appendix 29：PCAの考え方
set.seed(4)
x <- round(1:10 + rnorm(10, sd = 1), 0)
y <- round(1:10 + rnorm(10, sd = 3), 0)
  # 平均
mu_x <- mean(x)
mu_y <- mean(y)
  # 負荷量 (固有ベクトル)
data_df <- data.frame(X = x, Y = y)
  # 共分散行列を計算
covariance_matrix <- cov(data_df)
eigen_results <- eigen(covariance_matrix)
eigen_results$values / sum(eigen_results$values) # 寄与率
  ## [1] 0.91913391 0.08086609
  # PC1に対応する固有ベクトル
pc1_loading_x <- eigen_results$vectors[1, 1]  # Xの負荷量 (a11)
pc1_loading_y <- eigen_results$vectors[2, 1]  # Yの負荷量 (a12)
pc2_loading_x <- eigen_results$vectors[1, 2]  # Xの負荷量 (a21)
pc2_loading_y <- eigen_results$vectors[2, 2]  # Yの負荷量 (a22)
  # PC1軸の傾きと切片
slope_pc1 <- pc1_loading_y / pc1_loading_x
intercept_pc1 <- mu_y - slope_pc1 * mu_x
  # PC1スコアの計算する関数
calculate_pc1_score <- function(x_c, y_c) {
  return(pc1_loading_x * x_c + pc1_loading_y * y_c)
}
  # PC2スコアを計算する関数
calculate_pc2_score <- function(x_c, y_c) {
  return(pc2_loading_x * x_c + pc2_loading_y * y_c)
}
df_pca <- 
  tibble::tibble(x = x, y = y) |> 
  dplyr::mutate(
    x_c = x - mu_x,
    y_c = y - mu_y,
    Z1 = calculate_pc1_score(x_c, y_c),  # PCスコア
    Z2 = calculate_pc2_score(x_c, y_c),
    x1 = mu_x + Z1 * pc1_loading_x,      # 投影点 x1 = mu_x + Z1 * a11
    y1 = mu_y + Z1 * pc1_loading_y,      # 投影点 y1 = mu_y + Z1 * a12
  )
df_pca |>
  ggplot2::ggplot(ggplot2::aes(x, y)) +
  ggplot2::geom_point(size = 3) +
  ggplot2::geom_abline( # PCA第1軸
    intercept = intercept_pc1, 
    slope = slope_pc1, 
    linetype = "solid") +
  ggplot2::geom_segment( # PCA第1軸への垂線
    ggplot2::aes(x = x, y = y, xend = x1, yend = y1), 
    linetype = "dashed") +
  ggplot2::coord_fixed() + # 軸スケールの固定
  ggplot2::theme_bw()
  # (図6参照)


  # Appendix 30：環境傾度上に並んだデータ
val <- rep(c(1, 2, 3, 2, 1), 10)
sp <- paste0("sp", stringr::str_pad(unlist(purrr::map2(1:10, 5:14, `:`)), 2, "left", "0"))
st <- paste0("st", stringr::str_pad(rep(1:10, each = 5), 2, "left", "0"))
df_arch <- tibble::tibble(stand = st, species = sp, val = val)
tbl_arch <- ecan::df2table(df_arch, ab = "val")
t(tbl_arch)
  ##      st01 st02 st03 st04 st05 st06 st07 st08 st09 st10
  ## sp01    1    0    0    0    0    0    0    0    0    0
  ## sp02    2    1    0    0    0    0    0    0    0    0
  ## sp03    3    2    1    0    0    0    0    0    0    0
  ## sp04    2    3    2    1    0    0    0    0    0    0
  ## sp05    1    2    3    2    1    0    0    0    0    0
  ## sp06    0    1    2    3    2    1    0    0    0    0
  ## sp07    0    0    1    2    3    2    1    0    0    0
  ## sp08    0    0    0    1    2    3    2    1    0    0
  ## sp09    0    0    0    0    1    2    3    2    1    0
  ## sp10    0    0    0    0    0    1    2    3    2    1
  ## sp11    0    0    0    0    0    0    1    2    3    2
  ## sp12    0    0    0    0    0    0    0    1    2    3
  ## sp13    0    0    0    0    0    0    0    0    1    2
  ## sp14    0    0    0    0    0    0    0    0    0    1


  # Appendix 31：馬蹄効果の例
pca_arch <- 
  tbl_arch |>
  ecan::ordination(o_method = "pca")
gg_arch <- ecan::ord_plot(pca_arch)
gg_arch
  # (図7参照)


  # Appendix 32：Bray-Curtis距離をもとにしたnMDS
ord_nmds <- 
  df |>
  df2table(st = "stand", sp = "species", ab = "cover") |>
  ecan::ordination(
    o_method = "nmds", 
    d_method = "bray")
  # initial  value 14.412873 
  # iter   5 value 11.906739
  # iter  10 value 11.654365
  # final  value 11.588594 
  # converged


  # Appendix 33：nMDSの結果の抽出(地点)
ord_nmds_st <- 
  ecan::ord_extract_score(ord_nmds, score = "st_scores") |>
  tibble::tibble() |>
  print()
  ## # A tibble: 20 × 3
  ##         V1      V2 rowname
  ##      <dbl>   <dbl> <chr>  
  ##  1 -0.477  -0.343  1      
  ##  2 -0.221  -0.228  2      
  ##  3 -0.0514 -0.150  3      
  ##  4 -0.0499 -0.186  4      
  ##  5 -0.301  -0.0312 5      
  ## (省略)


  # Appendix 34：nMDSの結果の図示(地点)
ord_nmds_st |>
  ggplot2::ggplot(
    ggplot2::aes(V1, V2, label = .data[["rowname"]])) +
  ggplot2::geom_text() +
  ggplot2::theme_bw()
  # (図8参照)


  # Appendix 35：nMDSの結果の抽出(種)
indiv <- "species"
group <- "dummy_1"
ord_nmds_sp <- 
  ecan::ord_add_group( # 抽出も実行
    ord = ord_nmds, 
    score = "sp_scores", 
    df = df, 
    indiv = indiv, 
    group = group) |>
  print()
  ## Joining with `by = join_by(species)`
  ##                    V1           V2  species dummy_1 dummy_6
  ## Achimill -0.449534346 -0.009072400 Achimill       A       i
  ## Elymrepe -0.201703526  0.302774552 Elymrepe       E       e
  ## Lolipere -0.286289892  0.206386707 Lolipere       L       e
  ## Poaprat  -0.256076297  0.159569793  Poaprat       P       a
  ## Poatriv  -0.135961231  0.352651500  Poatriv       P       i
  ## (省略)


  # Appendix 36：nMDSの結果の色付きの図示(種)
ord_nmds_sp |>
  ggplot2::ggplot(
    ggplot2::aes(V1, V2, 
      label = .data[[indiv]])) +
  ggplot2::geom_point(
    ggplot2::aes(
      col = .data[[group]]), 
      alpha = 0.4, 
      size = 7) +
  ggplot2::geom_text() +
  ggplot2::theme_bw()
  ## (省略)


  # Appendix 37：Indicator Species Analysisでもとの結果の出力
isa_res_raw <- 
  ecan::ind_val(df, 
          group = "Management", 
          row_data = TRUE) # `row_data = TRUE`でlabdsv::indval()のそのままの結果を返す
isa_res_raw$relfrq # 種の出現頻度
  ##                  1         2   3         4
  ## Achimill 0.1666667 0.6666667 0.6 0.1666667
  ## Elymrepe 0.5000000 0.3333333 0.4 0.0000000
  ## Lolipere 0.5000000 1.0000000 1.0 0.1666667
  ## Poaprat  0.6666667 1.0000000 1.0 0.3333333
  ## Poatriv  1.0000000 0.6666667 1.0 0.0000000
  ## (省略)
isa_res_raw$relabu # 種の平均出現量
  ##                   1         2         3          4
  ## Achimill 0.04132231 0.5785124 0.2975207 0.08264463
  ## Elymrepe 0.37500000 0.2500000 0.3750000 0.00000000
  ## Lolipere 0.22500000 0.4500000 0.3000000 0.02500000
  ## Poaprat  0.23659306 0.3785489 0.3217666 0.06309148
  ## Poatriv  0.35532995 0.2791878 0.3654822 0.00000000
  ## (省略)
isa_res_raw$indval # 指標値
  ##                    1          2          3           4
  ## Achimill 0.006887052 0.38567493 0.17851240 0.013774105
  ## Elymrepe 0.187500000 0.08333333 0.15000000 0.000000000
  ## Lolipere 0.112500000 0.45000000 0.30000000 0.004166667
  ## Poaprat  0.157728707 0.37854890 0.32176656 0.021030494
  ## Poatriv  0.355329949 0.18612521 0.36548223 0.000000000
  ## (省略)
tibble(
  cls = isa_res_raw$maxcls,     # 最大値のクラス
  ind_val = isa_res_raw$indcls, # 指標値
  p = isa_res_raw$pval)         # p値
  ## # A tibble: 30 × 3
  ##      cls ind_val     p
  ##    <int>   <dbl> <dbl>
  ##  1     2   0.386 0.136
  ##  2     1   0.188 0.713
  ##  3     2   0.45  0.069
  ##  4     2   0.379 0.188
  ##  5     3   0.365 0.235
  ## (省略)


  # Appendix 38：Indicator Species Analysisでの整理した結果
group <- "Management"
(isa_res <- ecan::ind_val(df, group = group))
  ## Joining with `by = join_by(numeric_Management)`
  ## # A tibble: 30 × 4
  ##    Management species  ind.val p.value
  ##    <fct>      <chr>      <dbl>   <dbl>
  ##  1 SF         Elymrepe   0.188   0.689
  ##  2 SF         Alopgeni   0.547   0.046
  ##  3 SF         Agrostol   0.472   0.057
  ##  4 SF         Cirsarve   0.167   1    
  ##  5 SF         Sagiproc   0.241   0.513
  ## (省略)


  # Appendix 39：Indicator Species Analysisの結果の図示
isa_gg <- 
  isa_res |>
   ggplot2::ggplot(
    ggplot2::aes(
      x = .data[[group]], 
      y = .data[["ind.val"]],
      size = log(1 / (.data[["p.value"]] * 10)),
      label = .data[["species"]])) + 
  ggplot2::geom_point() + 
  ggrepel::geom_text_repel(
    ggplot2::aes(
      size = log(1 / (.data[["p.value"]] * 10), base = 5))) + 
  ggplot2::theme_bw() + 
    ggplot2::theme(legend.position = "none")
isa_gg
  # (図9参照)
