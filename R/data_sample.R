  # UI module
data_sampleInput <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("use_sample_data"), "Use sample data")
  )
}

  # Sever module
data_sampleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    eventReactive(input$use_sample_data,
      gen_sample_data()
    )
  })
}

  # Generate sample data
gen_sample_data <- function(){
  data(dune)
  data(dune.env)
  dune %>%
    table2df(st = "stand", sp = "species", ab = "cover") %>%
    dplyr::left_join(tibble::rownames_to_column(dune.env, "stand"))
}
