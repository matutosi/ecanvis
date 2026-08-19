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
