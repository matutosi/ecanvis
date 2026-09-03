test_df <- function(){
  data.frame(stand   = c("a", "a", "b", "b"),
             species = c("x", "y", "x", "y"),
             cover   = c(1, 2, 3, 4),
             group   = c("g1", "g1", "g2", "g2"),
             stringsAsFactors = FALSE)
}

test_that("indiv() is a column name even when the group is hidden", {
  skip_if_not_installed("shiny")

  shiny::testServer(ordinationSever,
                    args = list(data_in = test_df(), com_table = NULL), {
      # regression: see test-cluster.R
    session$setInputs(ord_show_group = FALSE, ord_use_species_scores = FALSE)
    expect_type(indiv(), "character")
    expect_equal(indiv(), "stand")

    session$setInputs(ord_use_species_scores = TRUE)
    expect_equal(indiv(), "species")
  })
})

test_that("score() switches between unit and item scores", {
  skip_if_not_installed("shiny")

  shiny::testServer(ordinationSever,
                    args = list(data_in = test_df(), com_table = NULL), {
    session$setInputs(ord_use_species_scores = FALSE)
    expect_equal(score(), "st_scores")
    session$setInputs(ord_use_species_scores = TRUE)
    expect_equal(score(), "sp_scores")
  })
})

test_that("an axis beyond the components of the method still draws", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ecan")
  skip_if_not_installed("vegan")

  data(dune, package = "vegan")

  shiny::testServer(ordinationSever,
                    args = list(data_in = ecan::table2df(dune),
                                com_table = dune), {
      # regression: "pcoa" returns two components, so names(ord_scores())[4]
      # was NA and ggplot stopped with "Must subset the data pronoun with a
      # string, not a character NA"
    session$setInputs(ord_o_method = "pcoa", ord_d_method = "bray",
                      ord_use_species_scores = FALSE, ord_show_group = FALSE,
                      ord_x = 1, ord_y = 4,
                      ggplot_alpha = 0.3, ggplot_point_size = 7)

    expect_length(axes(), 2)
    expect_no_error(gg())
    expect_no_error(output$ordination)
      # and the user is told which axis was drawn instead
    expect_match(output$ord_caution$html, "2 components")
  })
})

test_that("the axes are the scores, not the numeric columns of the group", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ecan")
  skip_if_not_installed("vegan")

  data(dune,     package = "vegan")
  data(dune.env, package = "vegan")
  df <- dplyr::left_join(ecan::table2df(dune),
                         tibble::rownames_to_column(dune.env, "stand"),
                         by = "stand")

  shiny::testServer(ordinationSever,
                    args = list(data_in = df, com_table = dune), {
      # A1 of dune.env is numeric, so it would look like an axis once
      # ord_add_group() had put it beside the scores
    session$setInputs(ord_o_method = "pcoa", ord_d_method = "bray",
                      ord_use_species_scores = FALSE, ord_show_group = TRUE,
                      ord_group = "A1", ord_x = 1, ord_y = 2,
                      ggplot_alpha = 0.3, ggplot_point_size = 7)

    expect_false("A1" %in% axes())
    expect_true("A1" %in% colnames(ord_scores()))
    expect_no_error(gg())
  })
})

test_that("no caution is shown while the axes are in range", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ecan")
  skip_if_not_installed("vegan")

  data(dune, package = "vegan")

  shiny::testServer(ordinationSever,
                    args = list(data_in = ecan::table2df(dune),
                                com_table = dune), {
    session$setInputs(ord_o_method = "dca", ord_d_method = "bray",
                      ord_use_species_scores = FALSE, ord_show_group = FALSE,
                      ord_x = 1, ord_y = 2,
                      ggplot_alpha = 0.3, ggplot_point_size = 7)
    expect_gte(length(axes()), 2)
    expect_null(output$ord_caution)
  })
})
