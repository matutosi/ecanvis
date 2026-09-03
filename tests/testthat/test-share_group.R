  # The cluster panels publish the TWINSPAN they made into a shared store, and
  # the ordination panels offer those as a group.  This is what lets the same
  # grouping be seen on a dendrogram and on an ordination.

shared_df <- function(){
  skip_if_not_installed("ecan")
  skip_if_not_installed("vegan")
  data(dune,     package = "vegan")
  data(dune.env, package = "vegan")
  dplyr::left_join(ecan::table2df(dune),
                   tibble::rownames_to_column(dune.env, "stand"),
                   by = "stand")
}

test_that("the cluster panel publishes its TWINSPAN under its own id", {
  skip_if_not_installed("shiny")

  data(dune, package = "vegan")
  store <- shiny::reactiveValues()

  shiny::testServer(clusterSever,
                    args = list(data_in = shared_df(), tbl = dune,
                                tw_store = store), {
    session$setInputs(cl_c_method = "twinspan", cl_d_method = "bray",
                      cls_with_sp = FALSE, cls_show_group = FALSE,
                      cls_label_gray = 0.3,
                      cls_tw_cut_levels = "0, 2, 5, 10, 20",
                      cls_tw_modified = FALSE, cls_tw_n_clusters = 0,
                      cls_tw_cells = "level")
      # testServer names the module itself, so the key is read back from `id`
    expect_s3_class(shiny::isolate(store[[id]]), "twinspan")
    expect_length(shiny::reactiveValuesToList(store), 1)

      # and takes them off again when the method is not twinspan
    session$setInputs(cl_c_method = "average")
    expect_null(shiny::isolate(store[[id]]))
  })
})

test_that("the ordination panel offers a group a cluster panel published", {
  skip_if_not_installed("shiny")

  data(dune, package = "vegan")
  df    <- shared_df()
  store <- shiny::reactiveValues()
  store[["cls_1"]] <- compute_cluster(dune, c_method = "twinspan")$twinspan

  shiny::testServer(ordinationSever,
                    args = list(data_in = df, com_table = dune,
                                tw_store = store), {
    session$setInputs(ord_o_method = "dca", ord_d_method = "bray",
                      ord_use_species_scores = FALSE, ord_show_group = TRUE,
                      ord_use_twinspan = FALSE,
                      ord_tw_cut_levels = "0, 2, 5, 10, 20",
                      ord_tw_modified = FALSE, ord_tw_n_clusters = 0,
                      ord_x = 1, ord_y = 2,
                      ggplot_alpha = 0.3, ggplot_point_size = 7)

      # the panel ran no TWINSPAN of its own, and still has the group
    expect_null(tw())
    expect_true("twinspan_cls_1" %in% colnames(group_df()))
    expect_false(any(is.na(group_df()$twinspan_cls_1)))

    choices <- ecan::cols_one2multi(group_df(), indiv(), include_self = FALSE)
    expect_true("twinspan_cls_1" %in% choices)

    session$setInputs(ord_group = "twinspan_cls_1")
    expect_true("twinspan_cls_1" %in% colnames(ord_scores()))
    expect_no_error(output$ordination)
  })
})

test_that("the shared group is the very one the cluster panel made", {
  skip_if_not_installed("shiny")

  data(dune, package = "vegan")
  df <- shared_df()
    # not `tw`: inside testServer that name is the module's own reactive
  tw_made <- compute_cluster(dune, c_method = "twinspan", n_clusters = 4)$twinspan
  store <- shiny::reactiveValues()
  store[["cls_2"]] <- tw_made

  shiny::testServer(ordinationSever,
                    args = list(data_in = df, com_table = dune,
                                tw_store = store), {
    session$setInputs(ord_o_method = "dca", ord_d_method = "bray",
                      ord_use_species_scores = FALSE, ord_show_group = TRUE,
                      ord_use_twinspan = FALSE,
                      ord_tw_cut_levels = "0, 2, 5, 10, 20",
                      ord_tw_modified = FALSE, ord_tw_n_clusters = 0,
                      ord_x = 1, ord_y = 2,
                      ggplot_alpha = 0.3, ggplot_point_size = 7)

    got <- unique(group_df()[, c("stand", "twinspan_cls_2")])
    want <- data.frame(stand = as.character(tw_made$classification$stand),
                       twinspan_cls_2 = as.character(tw_made$classification$group),
                       stringsAsFactors = FALSE)
    expect_setequal(paste(got$stand, got$twinspan_cls_2),
                    paste(want$stand, want$twinspan_cls_2))
  })
})

test_that("a group of stands is not offered while species are shown", {
  skip_if_not_installed("shiny")

  data(dune, package = "vegan")
  df    <- shared_df()
  store <- shiny::reactiveValues()
  store[["cls_1"]] <- compute_cluster(dune, c_method = "twinspan")$twinspan

  shiny::testServer(ordinationSever,
                    args = list(data_in = df, com_table = dune,
                                tw_store = store), {
      # the units of that TWINSPAN are stands, so it says nothing about
      # species: a column of NA would be a choice that colours nothing
    session$setInputs(ord_o_method = "dca", ord_d_method = "bray",
                      ord_use_species_scores = TRUE, ord_show_group = TRUE,
                      ord_use_twinspan = FALSE,
                      ord_tw_cut_levels = "0, 2, 5, 10, 20",
                      ord_tw_modified = FALSE, ord_tw_n_clusters = 0,
                      ord_x = 1, ord_y = 2,
                      ggplot_alpha = 0.3, ggplot_point_size = 7)
    expect_equal(indiv(), "species")
    expect_false("twinspan_cls_1" %in% colnames(group_df()))
  })
})

test_that("the panels still work without a store", {
  skip_if_not_installed("shiny")

  data(dune, package = "vegan")
  df <- shared_df()

  shiny::testServer(clusterSever,
                    args = list(data_in = df, tbl = dune), {
    session$setInputs(cl_c_method = "twinspan", cl_d_method = "bray",
                      cls_with_sp = FALSE, cls_show_group = FALSE,
                      cls_label_gray = 0.3,
                      cls_tw_cut_levels = "0, 2, 5, 10, 20",
                      cls_tw_modified = FALSE, cls_tw_n_clusters = 0,
                      cls_tw_cells = "level")
    expect_no_error(output$cluster)
  })

  shiny::testServer(ordinationSever,
                    args = list(data_in = df, com_table = dune), {
    session$setInputs(ord_o_method = "dca", ord_d_method = "bray",
                      ord_use_species_scores = FALSE, ord_show_group = TRUE,
                      ord_use_twinspan = TRUE, ord_group = "twinspan",
                      ord_tw_cut_levels = "0, 2, 5, 10, 20",
                      ord_tw_modified = FALSE, ord_tw_n_clusters = 0,
                      ord_x = 1, ord_y = 2,
                      ggplot_alpha = 0.3, ggplot_point_size = 7)
    expect_true("twinspan" %in% colnames(group_df()))
    expect_no_error(output$ordination)
  })
})

test_that("a cluster panel offers the groups another one published", {
  skip_if_not_installed("shiny")

  data(dune, package = "vegan")
  df    <- shared_df()
  store <- shiny::reactiveValues()
  store[["cls_1"]] <- compute_cluster(dune, c_method = "twinspan")$twinspan

  shiny::testServer(clusterSever,
                    args = list(data_in = df, tbl = dune, tw_store = store), {
      # this panel clusters some other way, and colours by the TWINSPAN of
      # cls_1: that is how two ways of classifying the stands are compared
    session$setInputs(cl_c_method = "ward.D2", cl_d_method = "bray",
                      cls_with_sp = FALSE, cls_show_group = TRUE,
                      cls_label_gray = 0.3,
                      cls_tw_cut_levels = "0, 2, 5, 10, 20",
                      cls_tw_modified = FALSE, cls_tw_n_clusters = 0)

    expect_null(cls_raw()$twinspan)
    expect_true("twinspan_cls_1" %in% colnames(group_df()))
    expect_true("twinspan_cls_1" %in%
                ecan::cols_one2multi(group_df(), indiv(), include_self = FALSE))

    session$setInputs(cls_group = "twinspan_cls_1")
    expect_no_error(output$cluster)
  })
})

test_that("a cluster panel does not offer its own group twice", {
  skip_if_not_installed("shiny")

  data(dune, package = "vegan")
  store <- shiny::reactiveValues()

  shiny::testServer(clusterSever,
                    args = list(data_in = shared_df(), tbl = dune,
                                tw_store = store), {
    session$setInputs(cl_c_method = "twinspan", cl_d_method = "bray",
                      cls_with_sp = FALSE, cls_show_group = TRUE,
                      cls_label_gray = 0.3,
                      cls_tw_cut_levels = "0, 2, 5, 10, 20",
                      cls_tw_modified = FALSE, cls_tw_n_clusters = 0)

      # it published under its own id, and reads that id back out again
    expect_s3_class(shiny::isolate(store[[id]]), "twinspan")
    expect_true("twinspan" %in% colnames(group_df()))
    expect_false(paste0("twinspan_", id) %in% colnames(group_df()))
      # regression: without setdiff(names, id) the same groups appeared twice
    expect_equal(sum(grepl("^twinspan", colnames(group_df()))), 1)
  })
})

test_that("the groups of two cluster panels can be told apart", {
  skip_if_not_installed("shiny")

  data(dune, package = "vegan")
  store <- shiny::reactiveValues()
  store[["cls_1"]] <- compute_cluster(dune, c_method = "twinspan",
                                      n_clusters = 2)$twinspan
  store[["cls_3"]] <- compute_cluster(dune, c_method = "twinspan",
                                      n_clusters = 5)$twinspan

  shiny::testServer(clusterSever,
                    args = list(data_in = shared_df(), tbl = dune,
                                tw_store = store), {
    session$setInputs(cl_c_method = "average", cl_d_method = "bray",
                      cls_with_sp = FALSE, cls_show_group = TRUE,
                      cls_label_gray = 0.3,
                      cls_tw_cut_levels = "0, 2, 5, 10, 20",
                      cls_tw_modified = FALSE, cls_tw_n_clusters = 0)

    df <- group_df()
    expect_true(all(c("twinspan_cls_1", "twinspan_cls_3") %in% colnames(df)))
      # each column carries the number of groups its own panel was set to
    expect_length(unique(df$twinspan_cls_1), 2)
    expect_length(unique(df$twinspan_cls_3), 5)
  })
})
