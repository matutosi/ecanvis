test_that("download reflects the current data, not the first one", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("readr")

  data <- shiny::reactiveVal(data.frame(a = 1))
  name <- shiny::reactiveVal("first")

  shiny::testServer(data_download_tsvServer,
                    args = list(data = data, filename = name), {
    path_1 <- output$data_download_tsv
    expect_equal(readr::read_tsv(path_1, show_col_types = FALSE)$a, 1)
    expect_equal(basename(path_1), "first.tsv")

      # regression: passing the value (not the reactive) froze both the data
      # and the file name at their first evaluation
    data(data.frame(a = 2))
    name("second")
    path_2 <- output$data_download_tsv
    expect_equal(readr::read_tsv(path_2, show_col_types = FALSE)$a, 2)
    expect_equal(basename(path_2), "second.tsv")
  })
})

test_that("plain values still work as data and filename", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("readr")

  shiny::testServer(data_download_tsvServer,
                    args = list(data = data.frame(a = 3), filename = "plain"), {
    path <- output$data_download_tsv
    expect_equal(readr::read_tsv(path, show_col_types = FALSE)$a, 3)
    expect_equal(basename(path), "plain.tsv")
  })
})
