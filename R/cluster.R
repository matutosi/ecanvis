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
            value = 0, min = 0, max = 64, step = 1),
          selectInput(ns("cls_tw_cells"), "Two-way table cells",
            choices = c("level", "abundance")),
          data_download_tsvUI(ns("download_two_way"))
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
        ),

        # The two-way table is what TWINSPAN is for, so it is shown with it.
        conditionalPanel(ns = ns,
          condition = "input.cl_c_method == 'twinspan'",
          shinycssloaders::withSpinner(type = sample(1:8, 1), color.background = "white",
            reactable::reactableOutput(ns("cls_two_way"))
          )
        )
      )

    )
  )
}

## Sever module
##   tw_store is a shared reactiveValues.  The panel puts the TWINSPAN it made
##   there under its own id, so that the ordination panels can colour by the
##   same groups instead of running TWINSPAN again with the same settings.
clusterSever <- function(id, data_in, tbl, tw_store = NULL){
  moduleServer(id, function(input, output, session){

    st <- reactive({ colnames(data_in)[1] })
    sp <- reactive({ colnames(data_in)[2] })
    ab <- reactive({ colnames(data_in)[3] })

    indiv <- eventReactive(c(input$cls_show_group, input$cls_with_sp), {
      pick_indiv(input$cls_with_sp, st(), sp())
    })

    # Compute
    cls_raw <- reactive({
      req(tbl)
      tbl %>%
        t_if_true(input$cls_with_sp) %>% # t() when chekcbox selected
        compute_cluster(c_method   = input$cl_c_method,
                        d_method   = input$cl_d_method,
                        modified   = input$cls_tw_modified,
                        n_clusters = as_n_clusters(input$cls_tw_n_clusters),
                        cut_levels = parse_cut_levels(input$cls_tw_cut_levels))
    })

    # The data, plus the groups TWINSPAN found so that they can be chosen too
    group_df <- reactive({
      add_tw_group(data_in, cls_raw()$twinspan, indiv())
    })

    # Publish for the other panels.  NULL when the method is not twinspan,
    # which is how a panel takes its groups back off the list.
    if(!is.null(tw_store)){
      observe({ tw_store[[id]] <- cls_raw()$twinspan })
    }

    # Update group select
    observeEvent(c(input$cls_show_group, indiv(), group_df()), {
      if(isTRUE(input$cls_show_group)){
        choices <- ecan::cols_one2multi(group_df(), indiv(), include_self = FALSE)
        updateSelectInput(session, "cls_group", choices = choices)
      }
    })

    # Plot
    output$cluster <- renderPlot(res = 96, {
      cls <- cls_raw()

        # without a group column the panel draws the plain dendrogram
        # rather than going blank
      df <- group_df()
      if(isTRUE(input$cls_show_group) && has_group(df, input$cls_group)){
        col <- ecan::cls_color(cls, df, indiv = indiv(), group = input$cls_group)  # need BEFORE add group
        cls <- ecan::cls_add_group(cls, df, indiv = indiv(), group = input$cls_group)
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

    # Two-way table (TWINSPAN only)
    two_way <- reactive({
      tw <- cls_raw()$twinspan
      req(tw)
        # the rows of the two-way table are the other side of the table
      row_name <- pick_indiv(!isTRUE(input$cls_with_sp), st(), sp())
      tw_two_way_df(tw, cells = input$cls_tw_cells, row_name = row_name)
    })

    output$cls_two_way <- reactable::renderReactable({
      req(two_way())
      reactable::reactable(two_way(), resizable = TRUE, filterable = TRUE,
                           searchable = TRUE, defaultPageSize = 25)
    })

    # Download the two-way table
    data_download_tsvServer("download_two_way",
      data = two_way,
      filename = reactive(paste("twinspan_two_way", st(), sp(), ab(), sep = "_")))

  })
}
  # devtools::load_all("../ecan/R")
