  # UI module
ordinationUI <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        # method
        selectInput(ns("or_o_method"), "ordination method",
          choices = c("pca", "ca", "dca",
                      "pcoa", "fspa", "nmds")
        ),
        selectInput(ns("or_d_method"), "distance method",
          choices = c("bray", "euclidean", "correlation", "manhattan",
                      "canberra", "clark", "kulczynski", "jaccard",
                      "gower", "altGower", "morisita", "horn",
                      "mountford", "raup", "binomial", "chao", "cao",
                      "mahalanobis", "chisq", "chord", "aitchison",
                      "robust.aitchison")
        ),
        # stand or species, x and y axis
        radioButtons(ns("or_score"), "scores for plot",
          choiceNames  = c("unit (stand)", "item (species)"),
          choiceValues = c("st_scores",    "sp_scores")
  #           choiceNames  = c("unit (stand)", "item (species)", "both"),
  #           choiceValues = c("st_scores",    "sp_scores",      "both")
        ),
        numericInput(ns("or_x"), "x axis component (1-4)",
          value = 1, min = 1, max = 4, step = 1,
        ),
        numericInput(ns("or_y"), "y axis component (1-4)",
          value = 2, min = 1, max = 4, step = 1,
        ),

        selectInput(ns("use_group"), "Use group", 
            choices = c("no"             = "no_group",
                        "unit (stand)"   = "st_group",
                        "item (species)" = "sp_group")

        ),
        selectInput(ns("group"), "unit group", choices = character(0)),

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

    observeEvent(input$use_group, ignoreInit = TRUE, {
      choices <- all_data$cols
      selected <- if(input$group == "") choices[1] else input$group
      updateSelectInput(session, "group", choices = choices, selected = selected)
    })

    output$ordination <- renderPlot(res = 96, {

      req(all_data$com_table)
      ord <-
        all_data$com_table %>%
        ordination(o_method = input$or_o_method, d_method = input$or_d_method)

      single <- 
        if(input$use_group == "st_group"){
          all_data$st
        } else if(input$use_group == "sp_group"){
          all_data$sp
        }

      ord_scores <- 
        ord_add_group(
        ord    = ord, 
        score  = input$or_score,
        df     = all_data$data_in,
        single = single,
        group  = input$group)
print(ord_scores)

      ggplot(ord_scores, aes(x = PC1, y = PC2, col = .data[[input$group]])) + 
        geom_point()

  # devtools::load_all("d:/matu/work/todo/ecan/R")
  # ord
  #       all_data$st
  #       all_data$sp
  #       all_data$ab
  #       all_data$cols
  # print(input$group)
  #       ord_plot(ord, score = input$or_score, x = input$or_x,  y = input$or_y, col = NULL)

    })
  })
}
