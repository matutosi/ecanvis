## UI module 
clusterUI <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(

        # method
        selectInput(ns("cl_c_method"), "cluster method",
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
        checkboxInput(ns("cls_with_sp"), "Cluster with item (species)", value = FALSE),

        # Show and select group
        checkboxInput(ns("cls_show_group"), "Show group"),
        selectInput(ns("cls_group"), "Select group", choices = character(0)),

      ),

      mainPanel(
        shinycssloaders::withSpinner(type = sample(1:8, 1), color.background = "white",
          plotOutput(ns("cluster"))
        )
      )

    )
  )
}

## Sever module
clusterSever <- function(id, data_in, st, sp, tbl){
  moduleServer(id, function(input, output, session){

    # Update group select
    indiv <- eventReactive(c(input$cls_show_group, input$cls_with_sp), {
      if(input$cls_show_group){
        indiv <- if(input$cls_with_sp){ sp } else { st }
        choices <- cols_one2multi(data_in, indiv, inculde_self = FALSE)
        updateSelectInput(session, "cls_group", choices = choices)
      }
      indiv
    })

    # Compute and Plot
    output$cluster <- renderPlot(res = 96, {
      req(tbl)
      cls <- 
        tbl %>%
        t_if_true(input$cls_with_sp) %>% # t() when chekcbox selected
        cluster(c_method = input$cl_c_method, d_method = input$cl_d_method)
      if(input$cls_show_group) cls$labels <- cls_add_group(cls, data_in, indiv = indiv(), group = input$cls_group)

      ggdendro::ggdendrogram(cls)
    })

  })
}
  # devtools::load_all("../ecan/R")
