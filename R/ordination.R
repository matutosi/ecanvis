  # UI module
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
        # stand or species, x and y axis
        selectInput(ns("ord_score"), "Scores for plot",
            choices = c("Unit (stand)"   = "st_scores",
                        "Item (species)" = "sp_scores")
         ),

        numericInput(ns("ord_x"), "X axis component (1-4)",
          value = 1, min = 1, max = 4, step = 1,
        ),
        numericInput(ns("ord_y"), "Y axis component (1-4)",
          value = 2, min = 1, max = 4, step = 1,
        ),

        selectInput(ns("ord_use_group"), "Use group", 
            choices = c("No group"       = "ord_no_group",
                        "Unit (stand)"   = "ord_st_group",
                        "Item (species)" = "ord_sp_group")

        ),
        selectInput(ns("ord_group"), "Select group", choices = character(0)),

      ),
      mainPanel(
        shinycssloaders::withSpinner(type = sample(1:8, 1), color.background = "white",
          plotOutput(ns("ordination"))
        )
      )
    )
  )
}

  # Server module
ordinationSever <- function(id, all_data){
  # ordinationSever <- function(id, data_in, st, sp, com_table){
  moduleServer(id, function(input, output, session){

    observeEvent(input$ord_use_group, ignoreInit = TRUE, { # Need "ignoreInit = TRUE"
      choices <- if(single() == "") { "" } else { cols_one2multi(all_data$data_in, single(), inculde_self = FALSE) }
      selected <- if(input$ord_group == "") choices[1] else input$ord_group
      updateSelectInput(session, "ord_group", choices = choices, selected = selected)
    })

    single <- eventReactive(input$ord_use_group, { # species or stand
      if(input$ord_use_group == "ord_st_group"){
        all_data$st
      } else if(input$ord_use_group == "ord_sp_group"){
        all_data$sp
      } else{
        ""
      }
    })


  # plot
    output$ordination <- renderPlot(res = 96, {

      ord <-
        all_data$com_table %>%
        ordination(o_method = input$ord_o_method, d_method = input$ord_d_method)

      ord_scores <- 
        if(input$ord_use_group == "ord_no_group"){
          ord_extract_score(ord, input$ord_score)
        } else {
          ord_add_group(
            ord    = ord, 
            score  = input$ord_score,
            df     = all_data$data_in,
            single = single(),
            group  = input$ord_group)
        }

      x   <- names(ord_scores)[input$ord_x]
      y   <- names(ord_scores)[input$ord_y]

      if(input$ord_use_group != "ord_no_group"){
        req(ord_scores, input$ord_group)
        gg <- 
          ggplot2::ggplot(ord_scores, ggplot2::aes(.data[[x]], .data[[y]], label = rownames(ord_scores))) +
          ggplot2::geom_point(aes(col = .data[[input$ord_group]]), alpha = 0.3, size = 7) +
          ggplot2::geom_text() +
          ggplot2::theme_bw()
      } else {
        gg <- 
          ggplot2::ggplot(ord_scores, ggplot2::aes(.data[[x]], .data[[y]], label = rownames(ord_scores))) +
          ggplot2::geom_text() + 
          ggplot2::theme_bw()
      }
    gg

    })

  })
}

  # devtools::load_all("../ecan/R")
