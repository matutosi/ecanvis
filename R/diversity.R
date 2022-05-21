calculate_diversity <- function(all_data){
  reactive({
    req(all_data$data_in)

    diversity <- 
      all_data$data_in %>%
      shdi(stand     = all_data$st,
           species   = all_data$sp,
           abundance = all_data$ab) %>%
      dplyr::mutate_if(is.numeric, round, digit = 4)

    cols <- cols_one2multi(all_data$data_in, all_data$st)
    extra_data <- 
      all_data$data_in %>%
      dplyr::select(all_of(cols)) %>%
      distinct()

    dplyr::left_join(diversity, extra_data)
  })
}

  # UI module 
diversityUI <- function(id, diversity){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("div_index"), "Diversity index",
          choices = c("Species richness (s)" = "s",
                      "Shannon's H' (h)"     = "h",
                      "Simpson's D (d)"      = "d", 
                      "Simpson's 1/d (i)"    = "i")
        ),
        checkboxInput(ns("use_st_group"), "Use unit (stand) group", value = FALSE),
        selectInput(ns("st_group"), "unit group", choices = character(0)),
      ),

      mainPanel(
        shinycssloaders::withSpinner(type = sample(1:8, 1), color.background = "white",
          plotOutput(ns("diversity_plot_s"))
        ),
      )
    )
  )
}

  # Sever module
diversitySever <- function(id, diversity){
  moduleServer(id, function(input, output, session){

    observeEvent(input$use_st_group, ignoreInit = TRUE, {
      choices <- setdiff(colnames(diversity), c("s", "h", "d", "i"))
      selected <- if(input$st_group == "") choices[1] else input$st_group
      updateSelectInput(session, "st_group", choices = choices, selected = selected)
    })

    output$diversity_plot_s <- renderPlot(res = 96, {
      if(input$use_st_group)
        div_gg <- ggplot(diversity, aes(x = .data[[input$st_group]], y = .data[[input$div_index]]))
      else
        div_gg <- ggplot(diversity, aes(x = 1,y = .data[[input$div_index]]))
      div_gg +
        geom_boxplot(outlier.shape = NA) +  # do not show outer point
        geom_jitter(height = 0, width = 0.1) + 
        theme_bw()
    })

  })
}
