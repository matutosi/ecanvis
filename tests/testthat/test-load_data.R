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

test_that("the panel holds the table back while the columns are duplicated", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("vegan")
  skip_if_not_installed("ecan")

  shiny::testServer(load_dataServer,
                    args = list(example_data = gen_example_data()), {
    session$setInputs(use_example = TRUE, file_s_jis = FALSE,
                      st = "stand", sp = "species", ab = "cover")
    expect_equal(colnames(session$returned())[1:3],
                 c("stand", "species", "cover"))

      # regression: relocate() dropped the repeated name, so the table came
      # back as stand, cover, species and every panel read cover as the item
    session$setInputs(sp = "stand")
    expect_match(output$dupulicated_caution$html, "NOT be duplicated")
    expect_error(session$returned(), class = "shiny.silent.error")

      # and it comes back once the choice is put right
    session$setInputs(sp = "species")
    expect_equal(colnames(session$returned())[1:3],
                 c("stand", "species", "cover"))
  })
})

test_that("the panel copes with the inputs not being set yet", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("vegan")
  skip_if_not_installed("ecan")

    # regression: if(input$use_example) stopped with "argument is of length
    # zero" before the checkboxes had sent their value
  shiny::testServer(load_dataServer,
                    args = list(example_data = gen_example_data()), {
    expect_error(data_in(), class = "shiny.silent.error")
    session$setInputs(use_example = TRUE)
    expect_s3_class(data_in(), "data.frame")
  })
})
