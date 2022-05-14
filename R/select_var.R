select_varInput <- function(id) {
  ns <- NS(id)
  selectInput(ns("var"), "Variable", choices = NULL) 
}

select_varServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    observeEvent(data(), {
      updateSelectInput(session, "var", choices = names(data()))
    })
    reactive(data()[[input$var]])
  })
}
