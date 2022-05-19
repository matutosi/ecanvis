  # UI module
data_exampleInput <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("use_example_data"), "Use example data")
  )
}

  # Sever module
data_exampleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    eventReactive(input$use_example_data,
      gen_example_data()
    )
  })
}

  # Generate example data
gen_example_data <- function(){
  data(dune)
  data(dune.env)
  sp_dammy <- 
    tibble::tibble(
      "species" = colnames(dune), 
      "dammy_1" = stringr::str_sub(colnames(dune), 1, 1),
      "dammy_6" = stringr::str_sub(colnames(dune), 6, 6)
    )
  dune %>%
    table2df(st = "stand", sp = "species", ab = "cover") %>%
    dplyr::left_join(tibble::rownames_to_column(dune.env, "stand")) %>%
    dplyr::left_join(sp_dammy)
}
