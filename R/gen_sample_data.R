  # generate sample data for download
gen_sample_data <- function(){
  data(dune)
  data(dune.env)
  dune %>%
    table2df(st = "stand", sp = "species", ab = "cover") %>%
    dplyr::left_join(tibble::rownames_to_column(dune.env, "stand"))
}
