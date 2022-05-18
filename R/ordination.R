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
        numericInput(ns("or_x"), "x axis component",
          value = 1, min = 1, max = 10, step = 1,
        ),
        numericInput(ns("or_y"), "y axis component",
          value = 2, min = 1, max = 10, step = 1,
        ),


  #   #   #   #   #   #   #   #   #   #   #   #   #   #   # 
  #   #   #   #   #   #   #   #   #   #   #   #   #   #   # 
  #   #   #   #   #   #   #   #   #   #   #   #   #   #   # 
        selectInput(ns("use_group"), "Use group", 
            choices = c("no"             = "no_group",
                        "unit (stand)"   = "st_group",
                        "item (species)" = "sp_group")

        ),
        selectInput(ns("group"), "unit group", choices = character(0)),
  #   #   #   #   #   #   #   #   #   #   #   #   #   #   # 
  #   #   #   #   #   #   #   #   #   #   #   #   #   #   # 
  #   #   #   #   #   #   #   #   #   #   #   #   #   #   # 

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
ordinationSever <- function(id, tbl, cols, data, st, sp){
  moduleServer(id, function(input, output, session){

  #   #   #   #   #   #   #   #   #   #   #   #   #   #   # 
  #   #   #   #   #   #   #   #   #   #   #   #   #   #   # 
  #   #   #   #   #   #   #   #   #   #   #   #   #   #   # 
    observeEvent(input$use_group, ignoreInit = TRUE, {
      choices <- cols
      selected <- if(input$group == "") choices[1] else input$group
      updateSelectInput(session, "group", choices = choices, selected = selected)
    })

    output$ordination <- renderPlot(res = 96, {
    req(tbl)

    ord <-
      tbl %>%
      ordination(o_method = input$or_o_method, d_method = input$or_d_method)

    group <- 
      if(input$use_group == "st_group"){
        as.character(st)
      } else if(input$use_group == "sp_group"){
        as.character(sp)
      }

print(  dplyr::distinct(data, .data[[group]], .data[[input$group]]) )
  #   #   #   #   #   #   #   #   #   #   #   #   #   #   # 
  #   #   #   #   #   #   #   #   #   #   #   #   #   #   # 
  #   #   #   #   #   #   #   #   #   #   #   #   #   #   # 



    ord_plot(ord, score = input$or_score, x = input$or_x,  y = input$or_y, col = input$group)
    })
  })
}
