  # Data with nothing but the unit, the item and the abundance has no column
  # that can be a group, so the group select is empty.  Every panel that offers
  # a group has to cope with that: ecan::ind_val() stops with
  # 'Needs "group" input', and a dendrogram or an ordination has nothing to
  # colour by.

minimal_df <- function(){
  skip_if_not_installed("ecan")
  skip_if_not_installed("vegan")
  data(dune, package = "vegan")
  ecan::table2df(dune)
}

test_that("the data really offers no group", {
  df <- minimal_df()
  expect_equal(ecan::cols_one2multi(df, "stand", include_self = FALSE),
               character(0))
})

test_that("the ISA panel explains itself instead of stopping", {
  skip_if_not_installed("shiny")

  df <- minimal_df()
  shiny::testServer(ind_valSever, args = list(data_in = df), {
    session$setInputs(p_val_max = "1", ind_val_range = c(0, 1))

      # regression: ecan::ind_val() stopped with 'Needs "group" input',
      # which showed as a red error in the panel
    expect_null(ind_val_res())
    expect_match(output$caution$html, "group")
      # the plot is skipped by req(), not stopped by an error the user sees
    expect_error(output$ind_val_plot, class = "shiny.silent.error")
  })
})

test_that("the ISA panel computes once a group is there", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("vegan")

  data(dune,     package = "vegan")
  data(dune.env, package = "vegan")
  df <- dplyr::left_join(minimal_df(),
                         tibble::rownames_to_column(dune.env, "stand"),
                         by = "stand")

  shiny::testServer(ind_valSever, args = list(data_in = df), {
    session$setInputs(p_val_max = "1", ind_val_range = c(0, 1),
                      ind_val_st_group = "Management")
    expect_s3_class(ind_val_res(), "data.frame")
    expect_null(output$caution)
  })
})

test_that("the cluster panel draws the plain dendrogram", {
  skip_if_not_installed("shiny")

  df <- minimal_df()
  data(dune, package = "vegan")

  shiny::testServer(clusterSever, args = list(data_in = df, tbl = dune), {
      # Show group is ticked but there is nothing to show
    session$setInputs(cl_c_method = "average", cl_d_method = "bray",
                      cls_with_sp = FALSE, cls_show_group = TRUE,
                      cls_label_gray = 0.3,
                      cls_tw_cut_levels = "0, 2, 5, 10, 20",
                      cls_tw_modified = FALSE, cls_tw_n_clusters = 0)
    expect_false(has_group(group_df(), input$cls_group))
    expect_no_error(output$cluster)
  })
})

test_that("the ordination panel falls back to the plain scores", {
  skip_if_not_installed("shiny")

  df <- minimal_df()
  data(dune, package = "vegan")

  shiny::testServer(ordinationSever,
                    args = list(data_in = df, com_table = dune), {
    session$setInputs(ord_o_method = "pcoa", ord_d_method = "bray",
                      ord_use_species_scores = FALSE, ord_show_group = TRUE,
                      ord_x = 1, ord_y = 2,
                      ggplot_alpha = 0.3, ggplot_point_size = 7)
    expect_false(has_group(data_in, input$ord_group))
    expect_equal(ord_scores(), ord_raw_scores())
    expect_no_error(output$ordination)
  })
})
