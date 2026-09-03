  # The ISA panel: the result, the filtering the plot applies, the table and
  # the download.  The behaviour when the data holds no group is in
  # test-no_group.R.

grouped_df <- function(){
  skip_if_not_installed("ecan")
  skip_if_not_installed("vegan")
  data(dune,     package = "vegan")
  data(dune.env, package = "vegan")
  dplyr::left_join(ecan::table2df(dune),
                   tibble::rownames_to_column(dune.env, "stand"),
                   by = "stand")
}

test_that("the result is one row per species and group, rounded", {
  skip_if_not_installed("shiny")

  df <- grouped_df()
  shiny::testServer(ind_valSever, args = list(data_in = df), {
    session$setInputs(p_val_max = "1", ind_val_range = c(0, 1),
                      ind_val_st_group = "Management")
    res <- ind_val_res()

    expect_s3_class(res, "data.frame")
      # ecan::ind_val() puts the group first, and the plot relies on that
    expect_equal(colnames(res)[1], "Management")
    expect_true(all(c("species", "ind.val", "p.value") %in% colnames(res)))
      # round_numeric() is applied, so nothing carries more than six digits
    expect_equal(res$ind.val, round(res$ind.val, 6))
  })
})

test_that("the plot filters by p.value and by the range of ind.val", {
  skip_if_not_installed("shiny")

  df <- grouped_df()
  shiny::testServer(ind_valSever, args = list(data_in = df), {
    session$setInputs(p_val_max = "1", ind_val_range = c(0, 1),
                      ind_val_st_group = "Management")
    res <- ind_val_res()

      # what the plot draws is the result put through filter_ind_val()
    wide   <- filter_ind_val(res, "1",    c(0, 1))
    narrow <- filter_ind_val(res, "0.05", c(0.5, 1))
    expect_equal(nrow(wide), nrow(res))
    expect_lt(nrow(narrow), nrow(wide))

    expect_no_error(output$ind_val_plot)
    session$setInputs(p_val_max = "0.05", ind_val_range = c(0.5, 1))
    expect_no_error(output$ind_val_plot)
  })
})

test_that("the plot copes with a filter that keeps nothing", {
  skip_if_not_installed("shiny")

  df <- grouped_df()
  shiny::testServer(ind_valSever, args = list(data_in = df), {
      # an empty range leaves no row at all
    session$setInputs(p_val_max = "0.001", ind_val_range = c(1, 1),
                      ind_val_st_group = "Management")
    expect_no_error(output$ind_val_plot)
  })
})

test_that("the table and the download follow the result", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("readr")

  df <- grouped_df()
  shiny::testServer(ind_valSever, args = list(data_in = df), {
    session$setInputs(p_val_max = "1", ind_val_range = c(0, 1),
                      ind_val_st_group = "Management")
    expect_no_error(output$ind_val_table)

      # the file is named after the columns and the group in use
    expect_match(basename(output[["download_tsv-data_download_tsv"]]),
                 "^ind_val_stand_species_abundance_Management[.]tsv$")
  })
})

test_that("an invalid set of columns is explained, not analysed", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ecan")
  skip_if_not_installed("vegan")

  df <- grouped_df()
    # the abundance column is not numeric here
  bad <- df[, c("stand", "species", "Management")]

  shiny::testServer(ind_valSever, args = list(data_in = bad), {
    session$setInputs(p_val_max = "1", ind_val_range = c(0, 1),
                      ind_val_st_group = "stand")
    expect_null(ind_val_res())
    expect_match(output$caution$html, "Abundance must be numeric")
  })
})
