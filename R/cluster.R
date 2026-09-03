## UI module 
clusterUI <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(

        # method
        selectInput(ns("cl_c_method"), "cluster method",
          choices = c("average", "ward.D", "ward.D2", "single",
                      "complete", "mcquitty", "median", "centroid", "diana",
                      "twinspan")
        ),

        # TWINSPAN divides the stands itself and uses no distance,
        # so the distance method is hidden and its own settings are shown.
        conditionalPanel(ns = ns,
          condition = "input.cl_c_method != 'twinspan'",
          selectInput(ns("cl_d_method"), "distance method",
            choices = c("bray", "euclidean", "correlation", "manhattan",
                        "canberra", "clark", "kulczynski", "jaccard",
                        "gower", "altGower", "morisita", "horn",
                        "mountford", "raup", "binomial", "chao", "cao",
                        "mahalanobis", "chisq", "chord", "aitchison",
                        "robust.aitchison")
          )
        ),

        conditionalPanel(ns = ns,
          condition = "input.cl_c_method == 'twinspan'",
          textInput(ns("cls_tw_cut_levels"), "Pseudospecies cut levels",
            value = "0, 2, 5, 10, 20"),
          checkboxInput(ns("cls_tw_modified"),
            "Modified TWINSPAN (divide the most heterogeneous group first)",
            value = FALSE),
          numericInput(ns("cls_tw_n_clusters"),
            "Number of groups (0: no limit)",
            value = 0, min = 0, max = 64, step = 1)
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
    ab <- reactive({ colnames(data_in)[3] })

    # Update group select
    indiv <- eventReactive(c(input$cls_show_group, input$cls_with_sp), {
      indiv <- pick_indiv(input$cls_with_sp, st(), sp())
      if(isTRUE(input$cls_show_group)){
        choices <- ecan::cols_one2multi(data_in, indiv, include_self = FALSE)
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
        compute_cluster(c_method   = input$cl_c_method,
                        d_method   = input$cl_d_method,
                        modified   = input$cls_tw_modified,
                        n_clusters = as_n_clusters(input$cls_tw_n_clusters),
                        cut_levels = parse_cut_levels(input$cls_tw_cut_levels))

      if(input$cls_show_group){
        col <- ecan::cls_color(cls, data_in, indiv = indiv(), group = input$cls_group)  # need BEFORE add group
        cls <- ecan::cls_add_group(cls, data_in, indiv = indiv(), group = input$cls_group)
        cls <- stats::as.dendrogram(cls)
        cls <- dendextend::`labels_colors<-`(cls, value = grDevices::gray(input$cls_label_gray))
        plot(cls)
        dendextend::colored_bars(colors = col, cls, input$cls_group, y_shift = 0,  y_scale = 2)
        graphics::par(new = TRUE)
        plot(cls)
      } else {
        cls <- stats::as.dendrogram(cls)
        plot(cls)
      }
    })

  })
}
  # devtools::load_all("../ecan/R")
