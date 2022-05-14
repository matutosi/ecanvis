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
ordinationSever <- function(id, df, st, sp, ab){
  moduleServer(id, function(input, output, session){
    output$ordination <- renderPlot(res = 96, {
    req(df)
          ord <-
            df %>%
            df2table(st = as.character(st),
                     sp = as.character(sp),
                     ab = as.character(ab)) %>%
            ordination(o_method = input$or_o_method, d_method = input$or_d_method)
          ord_plot(ord, score = input$or_score, x = input$or_x,  y = input$or_y)
    })
  })
}
