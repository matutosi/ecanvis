  # UI module 
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
        checkboxInput(ns("st_or_sp"), "clustering with item (species)", value = FALSE)
      ),
      mainPanel(
        shinycssloaders::withSpinner(type = sample(1:8, 1), color.background = "white",
          plotOutput(ns("clustering"))
        )
      )
    )
  )
}

  # Sever module
clusterSever <- function(id, tbl){
  moduleServer(id, function(input, output, session){
    output$clustering <- renderPlot(res = 96, {
      req(tbl)
      cls <- 
        tbl %>%
        t_if_true(input$st_or_sp) %>% # t() when chekcbox selected
        cluster(c_method = input$cl_c_method, d_method = input$cl_d_method)
      ggdendro::ggdendrogram(cls)
    })
  })
}
