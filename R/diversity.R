## Helper function
calculate_diversity <- function(df, st, sp, ab){
    diversity <- 
      df %>%
      shdi(stand     = st,
           species   = sp,
           abundance = ab) %>%
      dplyr::mutate_if(is.numeric, round, digit = 6)
    extra_data <- 
      df %>%
      select_one2multi(st, inculde_self = TRUE)

    dplyr::left_join(diversity, extra_data)
}

## UI module 
diversityUI <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(

        # Select index
        selectInput(ns("div_index"), "Diversity index",
          choices = c("Species richness (s)" = "s",
                      "Shannon's H' (h)"     = "h",
                      "Simpson's D (d)"      = "d", 
                      "Simpson's 1/d (i)"    = "i")
        ),

        # Select group
        checkboxInput(ns("div_show_st_group"), "Show unit (stand) group", value = FALSE),
        selectInput(ns("div_st_group"), "Unit group", choices = character(0)),

        # download data
        data_download_tsvUI(ns("download_tsv")),

      ),

      mainPanel(

        # Caution
        htmlOutput(ns("caution")),

        # Plot
        shinycssloaders::withSpinner(type = sample(1:8, 1), color.background = "white",
          plotOutput(ns("diversity_plot_s"))
        ),
      )
    ),
     # Table
    reactableOutput(ns("diversity_table"))
  )
}

## Sever module
diversitySever <- function(id, data_in){
  moduleServer(id, function(input, output, session){

    st <- reactive({ colnames(data_in)[1] })
    sp <- reactive({ colnames(data_in)[2] })
    ab <- reactive({ colnames(data_in)[3] })

    # Compute
    diversity <- reactive({
      
      if(st() != sp() & is.numeric(data_in[[ab()]])){
        output$caution <- renderUI(character(0)) # No caution
        if(st() != "" & sp() != "" & ab() != "" & !is.null(data_in[[ab()]]))
        calculate_diversity(data_in, st(), sp(), ab())
      } else {
        output$caution <-
          renderUI("Select correct set of unit, item and abundance. Unit and item must not be duplicated. Abundance must be numeric.")
      }
    })

    # Update group select
    observeEvent(c(diversity(), data_in, st(), sp(), ab(), input$div_show_st_group), {
      choices <- setdiff(colnames(diversity()), c("s", "h", "d", "i"))
      updateSelectInput(session, "div_st_group", choices = choices)
    })

    # Download data
    data_download_tsvServer("download_tsv", 
      data = diversity(),
      filename = paste("diversity", st(), sp(), ab(), sep = "_"))

    # Plot
    output$diversity_plot_s <- renderPlot(res = 96, {
      req(diversity(), input$div_st_group)

      # group setting
      all_data <- "all_data"
      selected_group <- if(input$div_show_st_group) { input$div_st_group } else { all_data }

      div <- 
        if(is.double(diversity()[[selected_group]])){
            dplyr::mutate(diversity(), {{selected_group}} := cut_conti(.data[[selected_group]]))
        } else {
          diversity()
        }
      div %>%
        dplyr::mutate({{all_data}} := 1) %>%
        ggplot(aes(x = .data[[selected_group]], y = .data[[input$div_index]])) + 
          ggplot2::geom_boxplot(outlier.shape = NA) +  # do not show outer point
          ggplot2::geom_jitter(height = 0, width = 0.1) + 
          ggplot2::theme_bw()
    })

    # Table
    output$diversity_table <- renderReactable({
      req(diversity())
      reactable::reactable(diversity(), resizable = TRUE, filterable = TRUE, searchable = TRUE,)
    })

  })
}
