## UI module 
ind_valUI <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(

      sidebarPanel(

        # Select group
        selectInput(ns("ind_val_st_group"), "Unit group", choices = character(0)),

        # Plot settings
        selectInput(ns("p_val_max"),   "Maximum p.value",
          choices = c("1", "0.1", "0.05", "0.01", "0.001", "0.001")),
        sliderInput(ns("ind_val_range"), "Range of ind.val", 
          min = 0, max = 1,  value = c(0, 1), step = 0.05),

        # download data
        data_download_tsvUI(ns("download_tsv")),

      ),

      mainPanel(
        # Caution
        htmlOutput(ns("caution")),

        # Plot
        shinycssloaders::withSpinner(type = sample(1:8, 1), color.background = "white",
          plotOutput(ns("ind_val_plot"))
        ),

        # Table
        shinycssloaders::withSpinner(type = sample(1:8, 1), color.background = "white",
          reactableOutput(ns("ind_val_table"))
        ),
      )

    )
  )
}

## Sever module
ind_valSever <- function(id, data_in){
  moduleServer(id, function(input, output, session){

    st <- reactive({ colnames(data_in)[1] })
    sp <- reactive({ colnames(data_in)[2] })
    ab <- reactive({ colnames(data_in)[3] })

    # Update group select
    observeEvent(c(data_in, st()), {
      choices <- cols_one2multi(data_in, st(), inculde_self = FALSE)
      updateSelectInput(session, "ind_val_st_group", choices = choices)
    })

    # Compute
    ind_val_res <- reactive({
      req(data_in)
      if(st() != sp() & is.numeric(data_in[[ab()]])){
        output$caution <- renderUI(character(0)) # No caution
        ind_val(df        = data_in, 
                stand     = st(), 
                species   = sp(), 
                abundance = ab(),
                group     = input$ind_val_st_group) %>%
        dplyr::mutate_if(is.numeric, round, digit = 6)
      } else {
        output$caution <- renderUI("Select correct set of unit, item and abundance. Unit and item must not be duplicated. Abundance must be numeric.")
      }
    })

    # Plot
    output$ind_val_plot <- renderPlot(res = 96, {
      req(ind_val_res(), input$ind_val_st_group)
      selected_group <- input$ind_val_st_group

      # group setting
      ind <- 
        if(is.double(ind_val_res()[[selected_group]])){
          dplyr::mutate(ind_val_res(), {{selected_group}} := cut_conti(.data[[selected_group]]))
        } else {
          ind_val_res()
        } %>%
        # filter by p.value and ind.val
        dplyr::filter(.data[["p.value"]] < as.numeric(input$p_val_max)) %>%
        dplyr::filter(.data[["ind.val"]] > input$ind_val_range[1]) %>%
        dplyr::filter(.data[["ind.val"]] < input$ind_val_range[2])
      ind %>%
        ggplot(aes(x = .data[[selected_group]], y = .data[["ind.val"]], 
                   size = log(1 / (.data[["p.value"]] * 10)),
                   label = .data[[sp()]])) + 
          geom_point() + 
          ggrepel::geom_text_repel(aes(size = log(1 / (.data[["p.value"]] * 10), base = 5))) + 
          theme_bw() + 
          theme(legend.position = "none")
    })

    # Download data
    data_download_tsvServer("download_tsv", 
      data = ind_val_res(),
      filename = paste("ind_val", st(), sp(), ab(), input$ind_val_st_group, sep = "_"))

    # Table
    output$ind_val_table <- renderReactable({
      req(ind_val_res())
      reactable::reactable(ind_val_res(), resizable = TRUE, filterable = TRUE, searchable = TRUE,)
    })

  })
}
