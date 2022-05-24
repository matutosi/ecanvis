## UI module 
ind_valUI <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(

      sidebarPanel(

        # Select group
        selectInput(ns("ind_val_st_group"), "Unit group", choices = character(0)),

        # download data
        data_download_tsvUI(ns("download_tsv")),

      ),

      mainPanel(
        # Caution
        htmlOutput(ns("caution")),

        # Table
        shinycssloaders::withSpinner(type = sample(1:8, 1), color.background = "white",
          reactableOutput(ns("ind_val_table"))
        ),
      )

    )
  )
}

## Sever module
ind_valSever <- function(id, data_in, st, sp, ab){
  moduleServer(id, function(input, output, session){
    # Update group select
    observeEvent(c(data_in, st), {
      choices <- cols_one2multi(data_in, st, inculde_self = FALSE)
      updateSelectInput(session, "ind_val_st_group", choices = choices)
    })

    # Compute
    ind_res <- reactive({
      req(data_in)
      if(st != sp & is.numeric(data_in[[ab]])){
        output$caution <- renderUI(character(0)) # No caution
        ind_val(df        = data_in, 
                stand     = st, 
                species   = sp, 
                abundance = ab,
                group     = input$ind_val_st_group) %>%
        dplyr::mutate_if(is.numeric, round, digit = 6)
      } else {
        output$caution <- renderUI("Select correct set of unit, item and abundance. Unit and item must not be duplicated. Abundance must be numeric.")
      }
    })

    # Download data
    data_download_tsvSever("download_tsv", 
      data = ind_res(),
      filename = paste("ind_val", st, sp, ab, input$ind_val_st_group, sep = "_"))

    output$ind_val_table <- renderReactable({
      req(ind_res())
      reactable::reactable(ind_res(), resizable = TRUE, filterable = TRUE, searchable = TRUE,)
    })

  })
}
  # devtools::install_github("matutosi/ecan")
  # devtools::load_all("../ecan/R")
