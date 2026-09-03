  # The UI functions are plain code: a stray comma or a function called without
  # its namespace only shows when they are built.  The app attaches reactable
  # and shinycssloaders through global.R, the tests do not, so a call without a
  # namespace fails here even though the app runs.

ui_html <- function(ui){
  paste(as.character(ui), collapse = "\n")
}

test_that("every UI module builds", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("reactable")
  skip_if_not_installed("shinycssloaders")

  for(f in c("load_dataUI", "diversityUI", "ind_valUI",
             "clusterUI", "ordinationUI")){
    ui <- expect_no_error(get(f)("test_id"))
    expect_s3_class(ui, "shiny.tag.list")
  }
})

test_that("the ids of a UI module are namespaced", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("reactable")
  skip_if_not_installed("shinycssloaders")

    # two panels of the same module are placed side by side in ui.R, so an id
    # that is not namespaced would be shared by both
  html <- ui_html(clusterUI("cls_1"))
  expect_true(grepl("cls_1-cl_c_method", html, fixed = TRUE))
  expect_true(grepl("cls_1-cls_two_way", html, fixed = TRUE))
  expect_false(grepl('"cl_c_method"', html, fixed = TRUE))
})

test_that("the cluster panel offers twinspan and hides the distance for it", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("reactable")
  skip_if_not_installed("shinycssloaders")

  html <- ui_html(clusterUI("cls_1"))
  expect_true(grepl(">twinspan<", html, fixed = TRUE))
    # the distance method and the twinspan settings are behind a condition
    # (the quotes of the condition are escaped in the html)
  expect_true(grepl("data-display-if=\"input.cl_c_method !=", html, fixed = TRUE))
  expect_true(grepl("data-display-if=\"input.cl_c_method ==", html, fixed = TRUE))
})

test_that("the ordination panel no longer offers fspa", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("reactable")
  skip_if_not_installed("shinycssloaders")

    # regression: "fspa" was removed from ecan 0.2.1 (package dave was archived)
  html <- ui_html(ordinationUI("ord_1"))
  expect_false(grepl("fspa", html, fixed = TRUE))
  expect_true(grepl(">pcoa<", html, fixed = TRUE))
  expect_true(grepl("ord_1-ord_caution", html, fixed = TRUE))
})

test_that("the ordination panel hides the TWINSPAN settings until they apply", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("reactable")
  skip_if_not_installed("shinycssloaders")

  html <- ui_html(ordinationUI("ord_1"))
  expect_true(grepl("ord_1-ord_use_twinspan", html, fixed = TRUE))
    # the settings are behind Show group, and then behind the checkbox itself
  expect_true(grepl('data-display-if="input.ord_show_group"', html, fixed = TRUE))
  expect_true(grepl('data-display-if="input.ord_use_twinspan"', html, fixed = TRUE))
})
