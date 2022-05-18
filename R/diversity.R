calculate_diversity <- function(data_in, st, sp, ab){
  reactive({
    req(data_in)
    data_in %>%
      shdi(stand     = st,
           species   = sp,
           abundance = ab) %>%
      dplyr::mutate_if(is.numeric, round, digit = 4) %>%
      dplyr::left_join(
        dplyr::select(data_in, !all_of(c(as.character(sp), as.character(ab)))) %>%
          dplyr::distinct()
      )
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
        geom_boxplot() + 
        geom_jitter(height = 0, width = 0.1) + 
        theme_bw()
    })

  })
}
