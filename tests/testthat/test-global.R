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
