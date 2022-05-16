  # UI module 
diversityUI <- function(id){
  ns <- NS(id)
  tagList(
    shinycssloaders::withSpinner(type = sample(1:8, 1), color.background = "white",
      plotOutput(ns("diversity_plot_s"))
    ),
    tableOutput(ns("diversity_table")),
  )
}

  # Sever module
diversitySever <- function(id, df, stand, species, abundance, group){
  moduleServer(id, function(input, output, session){

  # library(vegan)
  # data(dune)
  # data(dune.env)
  # tmp <- 
  #   table2df(dune) %>%
  #   dplyr::left_join(tibble::rownames_to_column(dune.env, "stand"))
  # df <- function() tmp

    diversity <- reactive({
  # print(df)
  # print(abundance)
      req(df)
      df %>%
        shdi(stand = stand, 
             species = species, 
             abundance = abundance) %>%
        dplyr::left_join(
          dplyr::distinct(df(), .data[[stand]], .data[[group]])
        )
    })

    output$diversity_plot_s <- renderPlot(res = 96, {
      diversity() %>%
        ggplot(aes(x = .data[[group]], y = s)) + 
          geom_boxplot() + 
          geom_jitter(width = 0.1) + 
          theme_bw()
    })

    output$diversity_table <- renderTable({
  # print(diversity())
  # print(class(diversity()))
  # print(as.data.frame(diversity()))
      diversity()
  #       diversity() %>%
  #         reactable::reactable(resizable = TRUE, filterable = TRUE, searchable = TRUE)
    })
  })
}
