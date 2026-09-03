test_that("calculate_diversity returns one row per unit with the four indices", {
  skip_if_not_installed("ecan")
  skip_if_not_installed("vegan")

  df  <- gen_example_data()
  res <- calculate_diversity(df, "stand", "species", "cover")

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), length(unique(df[["stand"]])))
  expect_true(all(c("stand", "s", "h", "d", "i") %in% colnames(res)))
    # the unit level columns of the input are carried over
  expect_true(all(c("A1", "Moisture", "Management") %in% colnames(res)))
})

test_that("calculate_diversity agrees with the definition of each index", {
  skip_if_not_installed("ecan")
  skip_if_not_installed("vegan")

  df  <- gen_example_data()
  res <- calculate_diversity(df, "stand", "species", "cover")

  one <- df[df[["stand"]] == df[["stand"]][1], ]
  p   <- one[["cover"]] / sum(one[["cover"]])
  got <- res[res[["stand"]] == df[["stand"]][1], ]

  expect_equal(got[["s"]], nrow(one))
  expect_equal(got[["h"]], round(-sum(p * log(p)), 6))
  expect_equal(got[["d"]], round(1 - sum(p ^ 2),   6))
  expect_equal(got[["i"]], round(1 / sum(p ^ 2),   6))
})

test_that("the plot grouping column is binned only when continuous", {
  skip_if_not_installed("ecan")
  skip_if_not_installed("vegan")

  res <- calculate_diversity(gen_example_data(), "stand", "species", "cover")
    # A1 is a continuous soil variable, Management is a categorical factor
  expect_false(is.factor(res[["A1"]]))
  expect_s3_class(cut_conti_col(res, "A1")[["A1"]], "factor")
  expect_identical(cut_conti_col(res, "Management")[["Management"]],
                   res[["Management"]])
})
