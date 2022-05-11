instructions <- 
  tags$ol(
    tags$h3('Instruction'),
    tags$li('Select "Use sample data" or "Upload file"'), 
    tags$li('Show data'),
    tags$li('Specify vars as inputs')
  )

data_up_show <- function(){
  tags$p(
    instructions,   # descriptions.R
    tags$hr(),
    checkboxInput("use_sample_data", "Use sample data", value = FALSE),
    tags$hr(),
    data_loadInput("upload_data"),
    tags$hr(),
    actionButton("show_data", "Show data"),
    tags$hr()
  )
}
