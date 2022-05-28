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
        sliderInput(ns("cls_label_gray"), "Darkness of labels (0: black, 1: white)",
          min = 0, max = 1, value = 0.3, step = 0.05),
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
clusterSever <- function(id, data_in, tbl){
  moduleServer(id, function(input, output, session){

    st <- reactive({ colnames(data_in)[1] })
    sp <- reactive({ colnames(data_in)[2] })

    # Update group select
    indiv <- eventReactive(c(input$cls_show_group, input$cls_with_sp), {
      if(input$cls_show_group){
        indiv <- if(input$cls_with_sp){ sp() } else { st() }
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
      if(input$cls_show_group){
        col <- cls_color(cls, data_in, indiv = indiv(), group = input$cls_group)  # need BEFORE add group
        cls <- cls_add_group(cls, data_in, indiv = indiv(), group = input$cls_group)
        cls <- stats::as.dendrogram(cls)
        labels_colors(cls) <- gray(input$cls_label_gray)
        plot(cls)
        dendextend::colored_bars(colors = col, cls, input$cls_group, y_shift = 0,  y_scale = 2)
        par(new = TRUE)
        plot(cls)
      } else {
        cls <- stats::as.dendrogram(cls)
        plot(cls)
      }
    })

  })
}
  # devtools::load_all("../ecan/R")
