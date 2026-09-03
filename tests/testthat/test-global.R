  # Read app_files out of a script without running it: global.R installs
  # packages when it is executed.
read_app_files <- function(path){
  exprs <- parse(path)
  for(e in exprs){
    if(is.call(e) && identical(as.character(e[[1]]), "<-") &&
       identical(as.character(e[[2]]), "app_files")){
      return(eval(e[[3]]))
    }
  }
  NULL
}

test_that("global.R sources every module file", {
  in_global <- read_app_files(file.path("..", "..", "R", "global.R"))
  expect_false(is.null(in_global))

    # global.R, ui.R and server.R are top level scripts, not module files
  on_disk <- setdiff(list.files(file.path("..", "..", "R"), pattern = "[.]R$"),
                     c("global.R", "ui.R", "server.R"))
  expect_setequal(in_global, on_disk)
})

test_that("the test helper sources the same files as global.R", {
    # app_files comes from helper-ecanvis.R
  expect_setequal(app_files,
                  read_app_files(file.path("..", "..", "R", "global.R")))
})

test_that("global.R installs ecan only when it is missing", {
    # deparse drops the comments, so only the code is checked
  src <- as.character(parse(file.path("..", "..", "R", "global.R")))
    # regression: install_github(force = TRUE) ran on every app start
  expect_false(any(grepl("force = TRUE", src, fixed = TRUE)))
  expect_true(any(grepl('requireNamespace("ecan"', src, fixed = TRUE)))
})

test_that("global.R attaches shiny and magrittr by name", {
  src <- as.character(parse(file.path("..", "..", "R", "global.R")))

    # regression: the packages were held in a variable and attached in a loop
    # (library(pkg, character.only = TRUE)).  rsconnect reads the code to
    # decide what to bundle for shinyapps.io, so none of them was visible and
    # the deployed app would start without them.
  expect_true(any(grepl("library(shiny)",    src, fixed = TRUE)))
  expect_true(any(grepl("library(magrittr)", src, fixed = TRUE)))
  expect_false(any(grepl("character.only",   src, fixed = TRUE)))
})

test_that("global.R does not install anything into a read only library", {
  src <- as.character(parse(file.path("..", "..", "R", "global.R")))
    # on shinyapps.io the library is built at deploy time and is not writable
  expect_true(any(grepl("file.access(.libPaths()[1], mode = 2)", src,
                        fixed = TRUE)))
})

test_that("rsconnect can see every package the app needs", {
  skip_if_not_installed("renv")

    # what rsconnect bundles for shinyapps.io is decided by scanning the code,
    # so a package it cannot see is a package the deployed app will not have
  found <- unique(renv::dependencies(file.path("..", "..", "R"),
                                     quiet = TRUE)$Package)

    # attached without a namespace
  expect_true(all(c("shiny", "magrittr") %in% found))
    # called as pkg::fun
  expect_true(all(c("ecan", "dplyr", "ggplot2", "ggrepel", "dendextend",
                    "reactable", "readr", "stringr", "tibble",
                    "shinycssloaders") %in% found))
    # named only in data(package = "vegan"), for the example data
  expect_true("vegan" %in% found)
})
