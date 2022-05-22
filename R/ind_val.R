  # UI module 
ind_valUI <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("ind_val_st_group"), "Unit group", choices = character(0)),
      ),

      mainPanel(
        shinycssloaders::withSpinner(type = sample(1:8, 1), color.background = "white",
          reactableOutput("ind_val_table")
        ),
      )
    )
  )
}

  # Sever module
diversitySever <- function(id, all_data){
  moduleServer(id, function(input, output, session){

    observeEvent(input$ind_val_st_group, {
      updateSelectInput()
    })

    output$ind_val_table <- renderReactable({
      ind_res <- 
        ind_val(
          df        = all_data$data_in, 
          stand     = all_data$st, 
          species   = all_data$sp, 
          abundance = all_data$ab,
          group     = input$st_group)
      reactable::reactable(ind_res, 
        resizable = TRUE, filterable = TRUE, searchable = TRUE,)
    })

  })
}
