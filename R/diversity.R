calculate_diversity <- function(data_in, st, sp, ab, st_gr){
  reactive({
    req(data_in)
    data_in %>%
      shdi(stand     = st,
           species   = sp,
           abundance = ab) %>%
      dplyr::left_join(
        dplyr::distinct(data_in, .data[[st]], .data[[st_gr]])
      )
  })
}

  # UI module 
diversityUI <- function(id){
  ns <- NS(id)
  tagList(
    shinycssloaders::withSpinner(type = sample(1:8, 1), color.background = "white",
      plotOutput(ns("diversity_plot_s"))
    ),
  )
}

  # Sever module
diversitySever <- function(id, diversity, group){
  moduleServer(id, function(input, output, session){
    output$diversity_plot_s <- renderPlot(res = 96, {
      diversity %>%
        ggplot(aes(x = .data[[group]], y = s)) + 
          geom_boxplot() + 
          geom_jitter(width = 0.1) + 
          theme_bw()
    })

  })
}
