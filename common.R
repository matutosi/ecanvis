  # Generate table
gen_table <- function(tbl, caption, caption_tex = caption){
  if(knitr::is_latex_output()){
    table <-   # pdf
      tbl |> 
      knitr::kable(format = "latex", booktabs = TRUE, caption = caption_tex)
  }else{
    table <-   # HTML, word
      tbl |>
      flextable::flextable() |>
      flextable::theme_vanilla() |>
      flextable::set_table_properties(align = "left", layout = "autofit") |>
      flextable::set_caption(caption)
  }
  table
}
  # Helper for gen_table()
str2tbl <- function(str, ncol){
  tbl <- matrix(str, ncol = ncol , byrow = TRUE)
  tbl |>
    magrittr::set_colnames(tbl[1,]) |>
    `[`(_, -1, ) |>
    tibble::as_tibble()
}
