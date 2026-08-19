test_that("gen_example_data builds a long format table", {
  skip_if_not_installed("vegan")
  skip_if_not_installed("ecan")

  df <- gen_example_data()
  expect_s3_class(df, "data.frame")
  expect_equal(colnames(df)[1:3], c("stand", "species", "cover"))
  expect_true(all(c("A1", "Moisture", "Management", "Use", "Manure",
                    "dummy_1", "dummy_6") %in% colnames(df)))
  expect_gt(nrow(df), 0)
  expect_true(is.numeric(df[["cover"]]))
    # zero abundances are dropped by table2df, so every row is a record
  expect_true(all(df[["cover"]] > 0))
})

test_that("gen_example_data does not depend on attached packages", {
  skip_if_not_installed("vegan")
  skip_if_not_installed("ecan")
    # regression: data(dune) without package = "vegan" only worked because
    # global.R had attached vegan beforehand
  expect_false("package:vegan" %in% search())
  expect_no_error(gen_example_data())
})

test_that("the example data is a valid input for the analyses", {
  skip_if_not_installed("vegan")
  skip_if_not_installed("ecan")

  df <- gen_example_data()
  expect_true(has_valid_cols(df, "stand", "species", "cover"))
})
