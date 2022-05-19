  # UI module
ordinationUI <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        # method
        selectInput(ns("ord_o_method"), "ordination method",
          choices = c("pca", "ca", "dca",
                      "pcoa", "fspa", "nmds")
        ),
        selectInput(ns("ord_d_method"), "distance method",
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
  #         radioButtons(ns("ord_score"), "scores for plot",
  #           choiceNames  = c("unit (stand)", "item (species)", "both"),
  #           choiceValues = c("st_scores",    "sp_scores",      "both"))
        numericInput(ns("ord_x"), "x axis component (1-4)",
          value = 1, min = 1, max = 4, step = 1,
        ),
        numericInput(ns("ord_y"), "y axis component (1-4)",
          value = 2, min = 1, max = 4, step = 1,
        ),

        selectInput(ns("ord_use_group"), "Group", 
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
  moduleServer(id, function(input, output, session){

    observeEvent(input$ord_use_group, ignoreInit = TRUE, {
      choices <- all_data$cols
      selected <- if(input$ord_group == "") choices[1] else input$ord_group
      updateSelectInput(session, "ord_group", choices = choices, selected = selected)
    })

    output$ordination <- renderPlot(res = 96, {

      ord <-
        all_data$com_table %>%
        ordination(o_method = input$ord_o_method, d_method = input$ord_d_method)

      single <- 
        if(input$ord_use_group == "ord_st_group"){
          all_data$st
        } else if(input$ord_use_group == "ord_sp_group"){
          all_data$sp
        }

  # [1] "st_scores"
  # # A tibble: 197 x 8
  #    stand species  cover    A1 Moisture Management Use      Manure
  #    <chr> <chr>    <dbl> <dbl> <ord>    <fct>      <ord>    <ord> 
  #  1 1     Achimill     1   2.8 1        SF         Haypastu 4     
  #  2 1     Elymrepe     4   2.8 1        SF         Haypastu 4     
  #  3 1     Lolipere     7   2.8 1        SF         Haypastu 4     
  #  4 1     Poaprat      4   2.8 1        SF         Haypastu 4     
  #  5 1     Poatriv      2   2.8 1        SF         Haypastu 4     
  #  6 2     Achimill     3   3.5 1        BF         Haypastu 2     
  #  7 2     Alopgeni     2   3.5 1        BF         Haypastu 2     
  #  8 2     Bellpere     3   3.5 1        BF         Haypastu 2     
  #  9 2     Bromhord     4   3.5 1        BF         Haypastu 2     
  # 10 2     Elymrepe     4   3.5 1        BF         Haypastu 2     
  # # ... with 187 more rows
  # [1] "species"
  # [1] "Manure"

  #   tmp <-    ord_add_group(
  #             ord    = ord, 
  #             score  = "st_scores",
  #             df     = df,
  #             single = "species",
  #             group  = "A1")

      ord_scores <- 
        if(input$ord_use_group != "ord_no_group"){
          ord_add_group(
            ord    = ord, 
            score  = input$ord_score,
            df     = all_data$data_in,
            single = single,
            group  = input$ord_group)
        } else {
          ord_extract_score(ord, input$ord_score)
        }

      has_group <- sum(stringr::str_count(colnames(ord_scores), input$ord_group)) > 0

      x   <- names(ord_scores)[input$ord_x]
      y   <- names(ord_scores)[input$ord_y]
      gg <- ggplot2::ggplot(ord_scores, ggplot2::aes(.data[[x]], .data[[y]], label = rownames(ord_scores)))
      if(input$ord_use_group != "ord_no_group" & has_group){
        gg <- gg + ggplot2::geom_point(aes(col = .data[[input$ord_group]]), alpha=0.2, size = 7)
        #   labs(col = input$ord_group) +  # change legend title
      }
      gg + 
        ggplot2::geom_text() + 
        ggplot2::theme_bw()

    })
  })
}

  # devtools::load_all("d:/matu/work/todo/ecan/R")
