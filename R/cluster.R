  # ui module for cluster
clusterUI <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        # method
        selectInput(ns("cl_c_method"), "clustering method",
          choices = c("average", "ward.D", "ward.D2", "single",
                      "complete", "mcquitty", "median", "centroid", "diana")
        ),
        selectInput(ns("cl_d_method"), "distance method",
          choices = c("bray", "euclidean", "correlation", "manhattan",
                      "canberra", "clark", "kulczynski", "jaccard",
                      "gower", "altGower", "morisita", "horn",
                      "mountford", "raup", "binomial", "chao", "cao",
                      "mahalanobis", "chisq", "chord", "aitchison",
                      "robust.aitchison")
        ),
        # stand or species
        checkboxInput(ns("st_or_sp"), "clustering with species", value = FALSE)
      ),
      mainPanel(
        shinycssloaders::withSpinner(type = sample(1:8, 1), color.background = "white",
          plotOutput(ns("clustering"))
        )
      )
    )
  )
}
  # server module for cluster
clusterSever <- function(id, df, st, sp, ab){
  moduleServer(id, function(input, output, session){
    output$clustering <- renderPlot(res = 96, {
      res <- try(silent = TRUE,
        if(!is.null(df)){
          cls <- 
            df %>%
            df2table(st = as.character(st), 
                     sp = as.character(sp), 
                     ab = as.character(ab)) %>%
            t_if_true(input$st_or_sp) %>% # t() when chekcbox selected
            clustering(c_method = input$cl_c_method, d_method = input$cl_d_method)
          ggdendro::ggdendrogram(cls)
        }
      )
      if(length(res) == 1) if(class(res) == "try-error") res <- NULL
      res
    })
  })
}
