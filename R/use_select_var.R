  #     # UI module
  # use_varUI <- function(id) {
  #   ns <- NS(id)
  #   uiOutput(ns("use_gr")) # checkbox
  # }
  # 
  # 
  # select_varUI <- function(id) {
  #   ns <- NS(id)
  #   uiOutput(ns("gr"))      # select var
  # }
  # 
  #     # Server module
  # use_varSever <- function(id, data, group_label = "unit group"){
  #   moduleServer(id, function(input, output, session){
  #     renderUI({ 
  #       req(data)
  #       checkboxInput("use_group", paste0("Use ", group_label), value = TRUE)
  #     })
  #   })
  # }
  # 
  # select_varSever <- function(id, data, group_label = "unit group", selected_col = 1){
  #   moduleServer(id, function(input, output, session){
  #     renderUI({
  #       req(data, input$use_group)
  #       if(input$use_group)
  #         varSelectInput("gr", paste0(group_label, ": "), data = data, selected = colnames(data)[selected_col])
  #     }) 
  #   })
  # }


  # UI module
use_select_varUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("use_gr")), # checkbox
    uiOutput(ns("gr"))      # select var
  )
}

  # Server module
use_select_varSever <- function(id, data, group_label = "unit group", selected_col = 1){
  moduleServer(id, function(input, output, session){
    ns <- NS(id)

    output$use_gr <- renderUI({ 
      req(data)
      checkboxInput(ns("use_group"), paste0("Use ", group_label), value = TRUE)
    })

    output$gr <- renderUI({
      req(data, input$use_group)
      if(input$use_group)
      varSelectInput(ns("gr"), paste0(group_label, ": "), data = data, selected = colnames(data)[selected_col])
    }) 
  })
}
