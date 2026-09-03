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
          choices = c("1", "0.1", "0.05", "0.01", "0.001")),
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
          reactable::reactableOutput(ns("ind_val_table"))
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
      choices <- ecan::cols_one2multi(data_in, st(), include_self = FALSE)
      updateSelectInput(session, "ind_val_st_group", choices = choices)
    })

    # Compute
    ind_val_res <- reactive({
      req(data_in)
      if(!has_valid_cols(data_in, st(), sp(), ab())){
        output$caution <- renderUI(msg_invalid_cols())
        NULL
      } else if(!has_group(data_in, input$ind_val_st_group)){
          # data with nothing but unit, item and abundance has no group,
          # and ecan::ind_val() stops with 'Needs "group" input'
        output$caution <- renderUI(msg_no_group())
        NULL
      } else {
        output$caution <- renderUI(character(0)) # No caution
        ecan::ind_val(df        = data_in, 
                stand     = st(), 
                species   = sp(), 
                abundance = ab(),
                group     = input$ind_val_st_group) %>%
        round_numeric()
      }
    })

    # Plot
    output$ind_val_plot <- renderPlot(res = 96, {
      req(ind_val_res())

      selected_group <- colnames(ind_val_res())[1]

      # group setting, then filter by p.value and ind.val
      ind <- 
        ind_val_res() %>%
        cut_conti_col(selected_group) %>%
        filter_ind_val(input$p_val_max, input$ind_val_range)
      ind %>%
        ggplot2::ggplot(ggplot2::aes(x = .data[[selected_group]], y = .data[["ind.val"]], 
                   size = log(1 / (.data[["p.value"]] * 10)),
                   label = .data[[sp()]])) + 
          ggplot2::geom_point() + 
          ggrepel::geom_text_repel(ggplot2::aes(size = log(1 / (.data[["p.value"]] * 10), base = 5))) + 
          ggplot2::theme_bw() + 
          ggplot2::theme(legend.position = "none")
    })

    # Download data
    data_download_tsvServer("download_tsv", 
      data = ind_val_res,
      filename = reactive(paste("ind_val", st(), sp(), ab(), input$ind_val_st_group, sep = "_")))

    # Table
    output$ind_val_table <- reactable::renderReactable({
      req(ind_val_res())
      reactable::reactable(ind_val_res(), resizable = TRUE, filterable = TRUE, searchable = TRUE,)
    })

  })
}
