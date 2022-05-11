## data_sample
data_sampleInput <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("use_sample_data"), "Use sample data")
  )
}

data_sampleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    eventReactive(input$use_sample_data,
      gen_sample_data()
    )
  })
}
