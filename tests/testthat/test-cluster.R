  # data_in for a module: the first three columns are unit, item and abundance
test_df <- function(){
  data.frame(stand   = c("a", "a", "b", "b"),
             species = c("x", "y", "x", "y"),
             cover   = c(1, 2, 3, 4),
             group   = c("g1", "g1", "g2", "g2"),
             stringsAsFactors = FALSE)
}

test_that("indiv() is a column name even when the group is hidden", {
  skip_if_not_installed("shiny")

  shiny::testServer(clusterSever,
                    args = list(data_in = test_df(), tbl = NULL), {
      # regression: the body assigned to a local `indiv` inside the if branch
      # only, so with the checkbox off the reactive returned itself
    session$setInputs(cls_show_group = FALSE, cls_with_sp = FALSE)
    expect_type(indiv(), "character")
    expect_equal(indiv(), "stand")

    session$setInputs(cls_with_sp = TRUE)
    expect_type(indiv(), "character")
    expect_equal(indiv(), "species")
  })
})

test_that("indiv() follows the item checkbox when the group is shown", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ecan")

  shiny::testServer(clusterSever,
                    args = list(data_in = test_df(), tbl = NULL), {
    session$setInputs(cls_show_group = TRUE, cls_with_sp = FALSE)
    expect_equal(indiv(), "stand")
    session$setInputs(cls_with_sp = TRUE)
    expect_equal(indiv(), "species")
  })
})

test_that("the panel draws with twinspan selected", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ecan")
  skip_if_not_installed("vegan")

  data(dune, package = "vegan")

  shiny::testServer(clusterSever,
                    args = list(data_in = ecan::table2df(dune), tbl = dune), {
      # the distance method is hidden for twinspan, but the input keeps its
      # value, so the panel has to draw whatever it holds
    session$setInputs(cl_c_method = "twinspan", cl_d_method = "bray",
                      cls_with_sp = FALSE, cls_show_group = FALSE,
                      cls_label_gray = 0.3,
                      cls_tw_cut_levels = "0, 2, 5, 10, 20",
                      cls_tw_modified = FALSE, cls_tw_n_clusters = 0)
    expect_no_error(output$cluster)

      # a half typed cut level must not stop the panel
    session$setInputs(cls_tw_cut_levels = "0, ")
    expect_no_error(output$cluster)

      # and the ordinary methods still draw
    session$setInputs(cl_c_method = "average", cls_tw_cut_levels = "0, 2, 5, 10, 20")
    expect_no_error(output$cluster)
  })
})

test_that("the panel offers the TWINSPAN groups in Show group", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ecan")
  skip_if_not_installed("vegan")

  data(dune, package = "vegan")
  data(dune.env, package = "vegan")
  df <- dplyr::left_join(ecan::table2df(dune),
                         tibble::rownames_to_column(dune.env, "stand"),
                         by = "stand")

  shiny::testServer(clusterSever, args = list(data_in = df, tbl = dune), {
    session$setInputs(cl_c_method = "twinspan", cl_d_method = "bray",
                      cls_with_sp = FALSE, cls_show_group = TRUE,
                      cls_label_gray = 0.3,
                      cls_tw_cut_levels = "0, 2, 5, 10, 20",
                      cls_tw_modified = FALSE, cls_tw_n_clusters = 0,
                      cls_tw_cells = "level")

      # the groups of the data are still there, and TWINSPAN adds its own
    choices <- ecan::cols_one2multi(group_df(), indiv(), include_self = FALSE)
    expect_true("Management" %in% choices)
    expect_true("twinspan"   %in% choices)

      # and the dendrogram draws when the TWINSPAN group is the one chosen
    session$setInputs(cls_group = "twinspan")
    expect_no_error(output$cluster)
  })
})

test_that("the panel does not offer a TWINSPAN group for the other methods", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ecan")
  skip_if_not_installed("vegan")

  data(dune, package = "vegan")

  shiny::testServer(clusterSever,
                    args = list(data_in = ecan::table2df(dune), tbl = dune), {
    session$setInputs(cl_c_method = "average", cl_d_method = "bray",
                      cls_with_sp = FALSE, cls_show_group = TRUE,
                      cls_label_gray = 0.3,
                      cls_tw_cut_levels = "0, 2, 5, 10, 20",
                      cls_tw_modified = FALSE, cls_tw_n_clusters = 0)
    expect_false("twinspan" %in% colnames(group_df()))
  })
})

test_that("the panel builds the two-way table for TWINSPAN only", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ecan")
  skip_if_not_installed("vegan")

  data(dune, package = "vegan")

  shiny::testServer(clusterSever,
                    args = list(data_in = ecan::table2df(dune), tbl = dune), {
    session$setInputs(cl_c_method = "twinspan", cl_d_method = "bray",
                      cls_with_sp = FALSE, cls_show_group = FALSE,
                      cls_label_gray = 0.3,
                      cls_tw_cut_levels = "0, 2, 5, 10, 20",
                      cls_tw_modified = FALSE, cls_tw_n_clusters = 0,
                      cls_tw_cells = "level")
      # the rows are the species, because the stands were clustered
    expect_equal(colnames(two_way())[1], "species")
    expect_no_error(output$cls_two_way)

      # clustering the items turns the table round
    session$setInputs(cls_with_sp = TRUE)
    expect_equal(colnames(two_way())[1], "stand")

      # and there is no two-way table for the other methods
    session$setInputs(cls_with_sp = FALSE, cl_c_method = "average")
    expect_error(two_way())
  })
})
