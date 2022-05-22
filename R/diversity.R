## Helper function
calculate_diversity <- function(df, st, sp, ab){
    diversity <- 
      df %>%
      shdi(stand     = st,
           species   = sp,
           abundance = ab) %>%
      dplyr::mutate_if(is.numeric, round, digit = 4)

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
        checkboxInput(ns("use_st_group"), "Use unit (stand) group", value = FALSE),
        selectInput(ns("st_group"), "Unit group", choices = character(0)),

      ),

      mainPanel(
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
diversitySever <- function(id, data_in, st, sp, ab){
  moduleServer(id, function(input, output, session){

    # Compute
    diversity <- reactive({
      if(st != "" & sp != "" & ab != "" & !is.null(data_in[[ab]]))
        calculate_diversity(data_in, st, sp, ab)
    })

    # Update group select
    observeEvent(c(data_in, st, sp, ab, input$use_st_group), ignoreInit = TRUE, {
      req(diversity())
      choices <- setdiff(colnames(diversity()), c("s", "h", "d", "i"))
      updateSelectInput(session, "st_group", choices = choices)
    })

    # Plot
    output$diversity_plot_s <- renderPlot(res = 96, {
      req(diversity())

      # group setting
      all_data <- "all_data"
      selected_group <- if(input$use_st_group) { input$st_group } else { all_data }

      diversity() %>%
        dplyr::mutate({{all_data}} := 1) %>%
        ggplot(aes(x = .data[[selected_group]], y = .data[[input$div_index]])) + 
          geom_boxplot(outlier.shape = NA) +  # do not show outer point
          geom_jitter(height = 0, width = 0.1) + 
          theme_bw()
    })

    # Table
    output$diversity_table <- renderReactable({
      req(diversity())
      reactable::reactable(diversity(), resizable = TRUE, filterable = TRUE, searchable = TRUE,)
    })

  })
}
