library(testthat)

  # The app files under R/ are plain scripts (they are deployed to shinyapps.io
  # as a shiny app directory, not installed as a package), so the tests source
  # them directly through tests/testthat/helper-ecanvis.R instead of using
  # test_check().
  # Run with:  Rscript tests/testthat.R
  #        or: Rscript -e 'testthat::test_dir("tests/testthat")'
test_dir(if(dir.exists("testthat")) "testthat" else "tests/testthat")
