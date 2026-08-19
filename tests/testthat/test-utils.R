test_that("cut_conti keeps every value including the minimum", {
  x <- c(0, 0.1, 0.5, 0.9, 1)
  res <- cut_conti(x)
  expect_s3_class(res, "factor")
  expect_length(res, length(x))
    # regression: cut() without include.lowest dropped the minimum
  expect_false(any(is.na(res)))
})

test_that("cut_conti bins a random vector without NA", {
  set.seed(1)
  x <- runif(100)
  expect_false(any(is.na(cut_conti(x))))
  expect_true(nlevels(cut_conti(x)) > 1)
})

test_that("is_conti treats integer as continuous", {
  expect_true(is_conti(1:5))         # regression: is.double() missed integers
  expect_true(is_conti(c(1.5, 2.5)))
  expect_false(is_conti(letters))
  expect_false(is_conti(factor(1:3)))
  expect_false(is_conti(NULL))
})

test_that("cut_conti_col bins only continuous columns", {
  df <- data.frame(num = 1:10, chr = letters[1:10], stringsAsFactors = FALSE)
  expect_s3_class(cut_conti_col(df, "num")$num, "factor")
  expect_type(cut_conti_col(df, "chr")$chr, "character")
})

test_that("cut_conti_col is a no-op for a missing column", {
  df <- data.frame(a = 1:3)
    # "all_data" is added after this call in the diversity module
  expect_identical(cut_conti_col(df, "all_data"), df)
  expect_identical(cut_conti_col(df, NULL), df)
})

test_that("pick_indiv selects unit or item", {
  expect_equal(pick_indiv(TRUE,  "stand", "species"), "species")
  expect_equal(pick_indiv(FALSE, "stand", "species"), "stand")
    # a checkbox is NULL before the UI is drawn
  expect_equal(pick_indiv(NULL,  "stand", "species"), "stand")
  expect_equal(pick_indiv(NA,    "stand", "species"), "stand")
})

test_that("has_valid_cols accepts a usable set of columns", {
  df <- data.frame(stand = c("a", "b"), species = c("x", "y"), cover = c(1, 2),
                   stringsAsFactors = FALSE)
  expect_true(has_valid_cols(df, "stand", "species", "cover"))
})

test_that("has_valid_cols rejects duplicated, missing or non numeric columns", {
  df <- data.frame(stand = c("a", "b"), species = c("x", "y"), cover = c(1, 2),
                   stringsAsFactors = FALSE)
  expect_false(has_valid_cols(df, "stand", "stand",   "cover"))    # duplicated
  expect_false(has_valid_cols(df, "stand", "species", "species"))  # duplicated
  expect_false(has_valid_cols(df, "stand", "species", "species"))
  expect_false(has_valid_cols(df, "stand", "species", "none"))     # absent
  expect_false(has_valid_cols(df, "stand", "species", "stand"))    # not numeric
  expect_false(has_valid_cols(df, "",      "species", "cover"))    # empty
  expect_false(has_valid_cols(df, NULL,    "species", "cover"))    # NULL
  expect_false(has_valid_cols(df, NA,      "species", "cover"))    # NA
})

test_that("filter_ind_val filters by p.value and ind.val", {
  df <- data.frame(p.value = c(0.001, 0.04, 0.5),
                   ind.val = c(0.9,   0.5,  0.1))
  expect_equal(nrow(filter_ind_val(df, 0.05, c(0, 1))), 2)
  expect_equal(nrow(filter_ind_val(df, 1,    c(0, 1))), 3)
  expect_equal(nrow(filter_ind_val(df, 1,    c(0.4, 1))), 2)
  expect_equal(nrow(filter_ind_val(df, 0.01, c(0, 1))), 1)
})

test_that("filter_ind_val keeps every row at the widest settings", {
    # regression: strict < and > dropped rows sitting exactly on the bounds
  df <- data.frame(p.value = c(1, 0.5), ind.val = c(1, 0))
  expect_equal(nrow(filter_ind_val(df, "1", c(0, 1))), 2)
})

test_that("filter_ind_val accepts a character p.value from selectInput", {
  df <- data.frame(p.value = c(0.001, 0.5), ind.val = c(0.9, 0.9))
  expect_equal(nrow(filter_ind_val(df, "0.01", c(0, 1))), 1)
})

test_that("filter_ind_val drops NA rows", {
  df <- data.frame(p.value = c(0.01, NA), ind.val = c(0.9, 0.9))
  expect_equal(nrow(filter_ind_val(df, 0.05, c(0, 1))), 1)
})

test_that("dots2list drops NULL and unwraps a single element", {
  expect_null(dots2list())
  expect_null(dots2list(NULL))
  expect_null(dots2list(NULL, NULL))
  expect_equal(dots2list(1), 1)
  expect_equal(dots2list(1, NULL), 1)
  expect_equal(dots2list(1, NULL, 2), list(1, 2))
})

test_that("t_if_true transposes only when TRUE", {
  m <- matrix(1:6, nrow = 2)
  expect_equal(t_if_true(m, TRUE), t(m))
  expect_equal(t_if_true(m, FALSE), m)
    # a checkbox is NULL before the UI is drawn
  expect_equal(t_if_true(m, NULL), m)
})

test_that("as_value forces a function but leaves a value alone", {
  expect_equal(as_value(1), 1)
  expect_equal(as_value(function() 1), 1)
})

test_that("msg_invalid_cols is a single string", {
  expect_type(msg_invalid_cols(), "character")
  expect_length(msg_invalid_cols(), 1)
})

test_that("has_duplicated_cols detects any repeated column name", {
  expect_false(has_duplicated_cols("stand", "species", "cover"))
  expect_true(has_duplicated_cols("stand", "stand",   "cover"))
  expect_true(has_duplicated_cols("stand", "species", "stand"))
  expect_true(has_duplicated_cols("stand", "species", "species"))
    # an input is NULL before the UI is drawn; c() drops it
  expect_false(has_duplicated_cols(NULL, "species", "cover"))
})
