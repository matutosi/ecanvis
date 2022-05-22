  # calculate_diversity <- function(df, st, sp, ab){
  #     diversity <- 
  #       df() %>%
  #       shdi(stand     = st,
  #            species   = sp,
  #            abundance = ab) %>%
  #       dplyr::mutate_if(is.numeric, round, digit = 4)
  # 
  #     extra_data <- 
  #       df() %>%
  #       select_one2multi(st, inculde_self = TRUE)
  # 
  #     dplyr::left_join(diversity, extra_data)
  # }
  # 
calculate_diversity <- function(all_data){
  diversity <- 
    all_data$data_in %>%
    shdi(stand     = all_data$st,
         species   = all_data$sp,
         abundance = all_data$ab) %>%
    dplyr::mutate_if(is.numeric, round, digit = 4)

  extra_data <- 
    all_data$data_in %>%
    select_one2multi(all_data$st, inculde_self = TRUE)

  dplyr::left_join(diversity, extra_data)
}

  # UI module 
diversityUI <- function(id){
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
        selectInput(ns("st_group"), "Unit group", choices = character(0)),
      ),

      mainPanel(
        shinycssloaders::withSpinner(type = sample(1:8, 1), color.background = "white",
          plotOutput(ns("diversity_plot_s"))
        ),
      )
    ),
    reactableOutput(ns("diversity_table"))
  )
}

  # Sever module
diversitySever <- function(id, all_data){
  # diversitySever <- function(id, diversity){
  moduleServer(id, function(input, output, session){

    diversity <- reactive({
      calculate_diversity(all_data)
    })
    

    observeEvent(input$use_st_group, ignoreInit = TRUE, {
      choices <- setdiff(colnames(diversity()), c("s", "h", "d", "i"))
      selected <- if(input$st_group == "") choices[1] else input$st_group
      updateSelectInput(session, "st_group", choices = choices, selected = selected)
    })

    output$diversity_plot_s <- renderPlot(res = 96, {
      if(input$use_st_group)
        div_gg <- ggplot(diversity(), aes(x = .data[[input$st_group]], y = .data[[input$div_index]]))
      else
        div_gg <- ggplot(diversity(), aes(x = 1,y = .data[[input$div_index]]))
      div_gg +
        geom_boxplot(outlier.shape = NA) +  # do not show outer point
        geom_jitter(height = 0, width = 0.1) + 
        theme_bw()
    })

    output$diversity_table <- renderReactable({
      reactable::reactable(diversity(), resizable = TRUE, filterable = TRUE, searchable = TRUE,)
    })

  })
}
  # devtools::load_all("d:/matu/work/todo/ecan/R")
