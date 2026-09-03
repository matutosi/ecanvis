  # Helpers that read the TWINSPAN settings of the cluster panel, and the
  # dispatch between ecan::cluster() and ecan::twinspan().

test_that("parse_cut_levels() reads a free text list of numbers", {
  expect_equal(parse_cut_levels("0, 2, 5, 10, 20"), c(0, 2, 5, 10, 20))
  expect_equal(parse_cut_levels("0 2 5"),           c(0, 2, 5))
  expect_equal(parse_cut_levels(" 5,2,0 "),         c(0, 2, 5))  # sorted
  expect_equal(parse_cut_levels("2, 2, 5"),         c(2, 5))     # de-duplicated
})

test_that("parse_cut_levels() falls back while the box is half typed", {
  default <- c(0, 2, 5, 10, 20)
  expect_equal(parse_cut_levels(NULL),   default)
  expect_equal(parse_cut_levels(""),     default)
  expect_equal(parse_cut_levels("   "),  default)
  expect_equal(parse_cut_levels("abc"),  default)
  expect_equal(parse_cut_levels("0, ,"), 0)  # keeps what is already numeric
})

test_that("as_n_clusters() turns 'no limit' into NULL", {
  expect_null(as_n_clusters(0))
  expect_null(as_n_clusters(NULL))
  expect_null(as_n_clusters(NA))
  expect_null(as_n_clusters(-1))
  expect_equal(as_n_clusters(4),   4L)
  expect_equal(as_n_clusters(4.7), 4L)
})

test_that("compute_cluster() keeps using ecan::cluster() for the other methods", {
  skip_if_not_installed("ecan")
  skip_if_not_installed("vegan")

  data(dune, package = "vegan")
  cls <- compute_cluster(dune, c_method = "average", d_method = "bray")

  expect_s3_class(cls, "hclust")
  expect_equal(cls$clustering_method, "average")
  expect_equal(cls$distance_method,   "bray")
  expect_null(cls$twinspan)
})

test_that("compute_cluster() runs TWINSPAN and returns an hclust", {
  skip_if_not_installed("ecan")
  skip_if_not_installed("vegan")

  data(dune, package = "vegan")
  cls <- compute_cluster(dune, c_method = "twinspan", d_method = "bray")

  expect_s3_class(cls, "hclust")
  expect_equal(cls$clustering_method, "twinspan")
    # TWINSPAN uses no distance, so the distance method must not be reported
  expect_null(cls$distance_method)
  expect_s3_class(cls$twinspan, "twinspan")
    # every stand is kept, and the labels are the stand names
  expect_setequal(cls$labels, rownames(dune))
})

test_that("compute_cluster() passes the TWINSPAN settings through", {
  skip_if_not_installed("ecan")
  skip_if_not_installed("vegan")

  data(dune, package = "vegan")
  cls <- compute_cluster(dune, c_method = "twinspan", d_method = "bray",
                         modified = TRUE, n_clusters = 4,
                         cut_levels = c(0, 2, 5))

  expect_equal(length(unique(cls$twinspan$classification$group)), 4)
  expect_true(cls$twinspan$modified)
  expect_equal(cls$twinspan$cut_levels, c(0, 2, 5))
})

test_that("the result of TWINSPAN works with the group helpers of ecan", {
  skip_if_not_installed("ecan")
  skip_if_not_installed("vegan")

    # cls_color() and cls_add_group() are called on the result in cluster.R,
    # so as.hclust() has to give them something they can use
  data(dune,     package = "vegan")
  data(dune.env, package = "vegan")
  df <- dplyr::left_join(ecan::table2df(dune),
                         tibble::rownames_to_column(dune.env, "stand"),
                         by = "stand")

  cls <- compute_cluster(dune, c_method = "twinspan", d_method = "bray")
  col <- ecan::cls_color(cls, df, indiv = "stand", group = "Management")
  expect_equal(length(col), nrow(dune))
  expect_false(any(is.na(col)))

  grouped <- ecan::cls_add_group(cls, df, indiv = "stand", group = "Management")
  expect_false(any(is.na(grouped$labels)))
})
