datasampleInput <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("use_sample_data"), "Use sample data")
  )
}

datasampleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    eventReactive(input$use_sample_data,
      gen_sample_data() 
    )
  })
}
