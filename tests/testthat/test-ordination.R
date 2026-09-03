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

test_that("the panel offers the TWINSPAN groups beside those of the data", {
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
    session$setInputs(ord_o_method = "dca", ord_d_method = "bray",
                      ord_use_species_scores = FALSE, ord_show_group = TRUE,
                      ord_use_twinspan = TRUE,
                      ord_tw_cut_levels = "0, 2, 5, 10, 20",
                      ord_tw_modified = FALSE, ord_tw_n_clusters = 0,
                      ord_x = 1, ord_y = 2,
                      ggplot_alpha = 0.3, ggplot_point_size = 7)

    choices <- ecan::cols_one2multi(group_df(), indiv(), include_self = FALSE)
    expect_true("Management" %in% choices)
    expect_true("twinspan"   %in% choices)

      # the group is carried into the scores and the plot draws
    session$setInputs(ord_group = "twinspan")
    expect_true("twinspan" %in% colnames(ord_scores()))
      # and it is not mistaken for an axis
    expect_false("twinspan" %in% axes())
    expect_no_error(output$ordination)
  })
})

test_that("no TWINSPAN group is added until it is asked for", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ecan")
  skip_if_not_installed("vegan")

  data(dune, package = "vegan")

  shiny::testServer(ordinationSever,
                    args = list(data_in = ecan::table2df(dune),
                                com_table = dune), {
    session$setInputs(ord_o_method = "dca", ord_d_method = "bray",
                      ord_use_species_scores = FALSE, ord_show_group = TRUE,
                      ord_use_twinspan = FALSE,
                      ord_tw_cut_levels = "0, 2, 5, 10, 20",
                      ord_tw_modified = FALSE, ord_tw_n_clusters = 0,
                      ord_x = 1, ord_y = 2,
                      ggplot_alpha = 0.3, ggplot_point_size = 7)
    expect_null(tw())
    expect_false("twinspan" %in% colnames(group_df()))
  })
})

test_that("the TWINSPAN group follows the species scores checkbox", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ecan")
  skip_if_not_installed("vegan")

  data(dune, package = "vegan")

  shiny::testServer(ordinationSever,
                    args = list(data_in = ecan::table2df(dune),
                                com_table = dune), {
      # with species scores the units are the species, so the table has to be
      # turned round before TWINSPAN is run on it
    session$setInputs(ord_o_method = "dca", ord_d_method = "bray",
                      ord_use_species_scores = TRUE, ord_show_group = TRUE,
                      ord_use_twinspan = TRUE,
                      ord_tw_cut_levels = "0, 2, 5, 10, 20",
                      ord_tw_modified = FALSE, ord_tw_n_clusters = 0,
                      ord_x = 1, ord_y = 2,
                      ggplot_alpha = 0.3, ggplot_point_size = 7)

    expect_equal(indiv(), "species")
    expect_setequal(tw()$classification$stand, colnames(dune))
    expect_true("twinspan" %in% colnames(group_df()))
    expect_false(any(is.na(group_df()$twinspan)))

    session$setInputs(ord_group = "twinspan")
    expect_no_error(output$ordination)
  })
})

test_that("the TWINSPAN settings of the panel are used", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ecan")
  skip_if_not_installed("vegan")

  data(dune, package = "vegan")

  shiny::testServer(ordinationSever,
                    args = list(data_in = ecan::table2df(dune),
                                com_table = dune), {
    session$setInputs(ord_o_method = "dca", ord_d_method = "bray",
                      ord_use_species_scores = FALSE, ord_show_group = TRUE,
                      ord_use_twinspan = TRUE,
                      ord_tw_cut_levels = "0, 2, 5",
                      ord_tw_modified = TRUE, ord_tw_n_clusters = 4,
                      ord_x = 1, ord_y = 2,
                      ggplot_alpha = 0.3, ggplot_point_size = 7)

    expect_equal(tw()$cut_levels, c(0, 2, 5))
    expect_true(tw()$modified)
    expect_length(unique(group_df()$twinspan), 4)
  })
})
