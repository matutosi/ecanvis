## UI module
ordinationUI <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(

        # method
        selectInput(ns("ord_o_method"), "Ordination method",
          choices = c("pca", "ca", "dca",
                      "pcoa", "fspa", "nmds")
        ),
        selectInput(ns("ord_d_method"), "Distance method",
          choices = c("bray", "euclidean", "correlation", "manhattan",
                      "canberra", "clark", "kulczynski", "jaccard",
                      "gower", "altGower", "morisita", "horn",
                      "mountford", "raup", "binomial", "chao", "cao",
                      "mahalanobis", "chisq", "chord", "aitchison",
                      "robust.aitchison")
        ),

        # x, y axis
        numericInput(ns("ord_x"), "X axis component (1-4)",
          value = 1, min = 1, max = 4, step = 1,),

        numericInput(ns("ord_y"), "Y axis component (1-4)",
          value = 2, min = 1, max = 4, step = 1,),

        # stand or species
        checkboxInput(ns("ord_use_species_scores"), "Use species scores"),

        # Japanese font
        selectInput(ns("jp_font"), "Japanese font", 
          choices = c("none", # system font
                      "IPAexGothic", "Source Han Sans",  "Noto Sans CJK JP",
                      "IPAexMincho", "Source Han Serif", "Noto Serif CJK JP")),

        # Show and select group
        checkboxInput(ns("ord_show_group"), "Show group"),
        selectInput(ns("ord_group"), "Select group", choices = character(0)),

        # ggplot controll
        sliderInput(ns("ggplot_point_size"), "Size of group circle (available in showing)", 
          min = 1, max = 10, value = 7, step = 0.5),
        sliderInput(ns("ggplot_alpha"), "Darkness of group circle (available in showing)", 
          min = 0, max = 1, value = 0.3, step = 0.05),

        # download data
        data_download_tsvUI(ns("download_tsv")),

      ),

      mainPanel(

        # Plot
        shinycssloaders::withSpinner(type = sample(1:8, 1), color.background = "white",
          plotOutput(ns("ordination"))
        ),

      )
    )
  )
}

## Server module
ordinationSever <- function(id, data_in, com_table){
  moduleServer(id, function(input, output, session){

    st <- reactive({ colnames(data_in)[1] })
    sp <- reactive({ colnames(data_in)[2] })
    ab <- reactive({ colnames(data_in)[3] })

    # Update group select
    indiv <- eventReactive(c(input$ord_show_group, input$ord_use_species_scores), {
      if(input$ord_show_group){
        indiv <- if(input$ord_use_species_scores){ sp() } else { st() }
        choices <- cols_one2multi(data_in, indiv, inculde_self = FALSE)
        updateSelectInput(session, "ord_group", choices = choices)
      }
      indiv
    })

    # Compute
    ord_scores <- reactive({
        ord <-
          com_table %>%
          ordination(o_method = input$ord_o_method, d_method = input$ord_d_method)

        score <- if(input$ord_use_species_scores) "sp_scores" else "st_scores"
        ord_scores <- 
          if(input$ord_show_group){
            ord_add_group(
              ord    = ord, 
              score  = score,
              df     = data_in,
              indiv = indiv(),    # need "()": indiv is reactive
              group  = input$ord_group)
          } else {
            row_name <- if(input$ord_use_species_scores){ sp() } else { st() }
            ord_extract_score(ord, score, row_name)
          }

        # Download data
        data_download_tsvServer("download_tsv", 
          data = ord_scores,
          filename = paste("ord", score, st(), sp(), ab(), input$ord_o_method, input$ord_d_method, sep = "_"))

        # return
        ord_scores
    })

    # Plot
    gg <- reactive({
      # settings
      font_family <- if(input$jp_font == "none") "" else input$jp_font
      x   <- names(ord_scores())[input$ord_x]
      y   <- names(ord_scores())[input$ord_y]

      if(input$ord_show_group){
        req(input$ord_group)
        alpha <- input$ggplot_alpha
        size  <- input$ggplot_point_size

        gg <- 
          ggplot2::ggplot(ord_scores(), ggplot2::aes(.data[[x]], .data[[y]], label = rownames(ord_scores()))) +
          ggplot2::geom_point(aes(col = .data[[input$ord_group]]), alpha = alpha, size = size) +
          ggplot2::geom_text() +
          ggplot2::theme_bw(base_family = font_family)
      } else {
        gg <- 
          ggplot2::ggplot(ord_scores(), ggplot2::aes(.data[[x]], .data[[y]], label = rownames(ord_scores()))) +
          ggplot2::geom_text() + 
          ggplot2::theme_bw(base_family = font_family)
      }
      gg
    })

    # Render
    output$ordination <- renderPlot(res = 96, {
      gg()
    })

  })
}
  # devtools::load_all("../ecan/R")
