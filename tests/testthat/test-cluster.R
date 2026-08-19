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
