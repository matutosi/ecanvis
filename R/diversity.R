calculate_diversity <- function(data_in, st, sp, ab){
  reactive({
    req(data_in)
    data_in %>%
      shdi(stand     = st,
           species   = sp,
           abundance = ab) %>%
      dplyr::left_join(
        dplyr::select(data_in, !all_of(c(as.character(sp), as.character(ab))))
      )
  })
}

  # UI module 
diversityUI <- function(id, diversity){
  ns <- NS(id)
  tagList(
    checkboxInput(ns("use_st_group"), "Use unit (stand) group", value = FALSE),
    selectInput(ns("st_group"), "unit group", choices = character(0)),

    shinycssloaders::withSpinner(type = sample(1:8, 1), color.background = "white",
      plotOutput(ns("diversity_plot_s"))
    ),
  )
}

  # Sever module
diversitySever <- function(id, diversity){
  moduleServer(id, function(input, output, session){

    observeEvent(input$use_st_group, ignoreInit = TRUE, {
      updateSelectInput(session, "st_group", choices = colnames(diversity))
    })

    output$diversity_plot_s <- renderPlot(res = 96, {
      diversity %>%
        ggplot(aes(x = .data[[input$st_group]], y = s)) + 
          geom_boxplot() + 
          geom_jitter(width = 0.1) + 
          theme_bw()
    })

  })
}

  # UI module 
  # diversityUI <- function(id){
  #   ns <- NS(id)
  #   tagList(
  #     shinycssloaders::withSpinner(type = sample(1:8, 1), color.background = "white",
  #       plotOutput(ns("diversity_plot_s"))
  #     ),
  #   )
  # }
  # 
  # Sever module
  # diversitySever <- function(id, diversity, group){
  #   moduleServer(id, function(input, output, session){
  #     output$diversity_plot_s <- renderPlot(res = 96, {
  #       diversity %>%
  #         ggplot(aes(x = .data[[group]], y = s)) + 
  #           geom_boxplot() + 
  #           geom_jitter(width = 0.1) + 
  #           theme_bw()
  #     })
  #   })
  # }
