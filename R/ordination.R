## UI module
ordinationUI <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(

        # method
        selectInput(ns("ord_o_method"), "Ordination method",
          # "fspa" was removed from ecan 0.2.1: package dave was archived.
          choices = c("pca", "ca", "dca", "pcoa", "nmds")
        ),
        selectInput(ns("ord_d_method"), "Distance method",
          choices = c("bray", "euclidean", "correlation", "manhattan",
                      "canberra", "clark", "kulczynski", "jaccard",
                      "gower", "altGower", "morisita", "horn",
                      "mountford", "raup", "binomial", "chao", "cao",
                      "mahalanobis", "chisq", "chord", "aitchison",
                      "robust.aitchison")
        ),

        # x, y axis
        numericInput(ns("ord_x"), "X axis component (1-4)",
          value = 1, min = 1, max = 4, step = 1,),

        numericInput(ns("ord_y"), "Y axis component (1-4)",
          value = 2, min = 1, max = 4, step = 1,),

        # stand or species
        checkboxInput(ns("ord_use_species_scores"), "Use species scores"),

        # Show and select group
        checkboxInput(ns("ord_show_group"), "Show group"),

        # TWINSPAN makes groups of its own, which are put beside the columns
        # of the data so that they can be chosen in the same way.
        conditionalPanel(ns = ns,
          condition = "input.ord_show_group",
          checkboxInput(ns("ord_use_twinspan"), "Add TWINSPAN group",
            value = FALSE),
          conditionalPanel(ns = ns,
            condition = "input.ord_use_twinspan",
            textInput(ns("ord_tw_cut_levels"), "Pseudospecies cut levels",
              value = "0, 2, 5, 10, 20"),
            checkboxInput(ns("ord_tw_modified"),
              "Modified TWINSPAN (divide the most heterogeneous group first)",
              value = FALSE),
            numericInput(ns("ord_tw_n_clusters"),
              "Number of groups (0: no limit)",
              value = 0, min = 0, max = 64, step = 1)
          )
        ),
        selectInput(ns("ord_group"), "Select group", choices = character(0)),

        # ggplot controll
        sliderInput(ns("ggplot_point_size"), "Size of group circle (available in showing)", 
          min = 1, max = 10, value = 7, step = 0.5),
        sliderInput(ns("ggplot_alpha"), "Darkness of group circle (available in showing)", 
          min = 0, max = 1, value = 0.3, step = 0.05),

        # download data
        data_download_tsvUI(ns("download_tsv")),

      ),

      mainPanel(

        # Caution
        htmlOutput(ns("ord_caution")),

        # Plot
        shinycssloaders::withSpinner(type = sample(1:8, 1), color.background = "white",
          plotOutput(ns("ordination"))
        ),

      )
    )
  )
}

## Server module
##   tw_store is the shared reactiveValues the cluster panels write to.  Every
##   TWINSPAN they made is offered here as one more group column, so that the
##   same grouping can be seen on a dendrogram and on an ordination.
ordinationSever <- function(id, data_in, com_table, tw_store = NULL){
  moduleServer(id, function(input, output, session){

    st <- reactive({ colnames(data_in)[1] })
    sp <- reactive({ colnames(data_in)[2] })
    ab <- reactive({ colnames(data_in)[3] })

    indiv <- eventReactive(c(input$ord_show_group, input$ord_use_species_scores), {
      pick_indiv(input$ord_use_species_scores, st(), sp())
    })

      # TWINSPAN on the table the scores belong to: with species scores the
      # units are the species, so the table is turned round first, as the
      # cluster panel does for "Cluster with item".
    tw <- reactive({
      if(!isTRUE(input$ord_show_group) || !isTRUE(input$ord_use_twinspan))
        return(NULL)
      req(com_table)
      cls <-
        com_table %>%
        t_if_true(input$ord_use_species_scores) %>%
        compute_cluster(c_method   = "twinspan",
                        modified   = input$ord_tw_modified,
                        n_clusters = as_n_clusters(input$ord_tw_n_clusters),
                        cut_levels = parse_cut_levels(input$ord_tw_cut_levels))
      cls$twinspan
    })

    # The data, plus the groups TWINSPAN found so that they can be chosen too:
    # the one this panel made, and the ones the cluster panels published.
    group_df <- reactive({
      df <- add_tw_group(data_in, tw(), indiv())
      if(is.null(tw_store)) return(df)
      shared <- reactiveValuesToList(tw_store)
      for(nm in sort(names(shared)))
          # a TWINSPAN of stands says nothing about species, and add_tw_group()
          # leaves the data alone when the units do not match
        df <- add_tw_group(df, shared[[nm]], indiv(), col = paste0("twinspan_", nm))
      df
    })

    # Update group select
    observeEvent(c(input$ord_show_group, indiv(), group_df()), {
      if(isTRUE(input$ord_show_group)){
        choices <- ecan::cols_one2multi(group_df(), indiv(), include_self = FALSE)
        updateSelectInput(session, "ord_group", choices = choices)
      }
    })

    # Compute
    score <- reactive({
      if(isTRUE(input$ord_use_species_scores)) "sp_scores" else "st_scores"
    })

    ord <- reactive({
      req(com_table)
      com_table %>%
        ecan::ordination(o_method = input$ord_o_method, d_method = input$ord_d_method)
    })

      # The scores on their own.  The axes are taken from here rather than from
      # ord_scores(), because ord_add_group() adds columns that are numeric too
      # (A1 of dune.env, say) and those are not axes.
    ord_raw_scores <- reactive({
      row_name <- pick_indiv(input$ord_use_species_scores, st(), sp())
      ecan::ord_extract_score(ord(), score(), row_name)
    })

    axes <- reactive({ score_axes(ord_raw_scores()) })

    ord_scores <- reactive({
        if(isTRUE(input$ord_show_group) && has_group(group_df(), input$ord_group)){
          ecan::ord_add_group(
            ord    = ord(), 
            score  = score(),
            df     = group_df(),
            indiv = indiv(),    # need "()": indiv is reactive
            group  = input$ord_group)
        } else {
          ord_raw_scores()
        }
    })

    # Download data (registered once, values are forced at download time)
    data_download_tsvServer("download_tsv", 
      data = ord_scores,
      filename = reactive(
        paste("ord", score(), st(), sp(), ab(),
              input$ord_o_method, input$ord_d_method, sep = "_")))

    # Caution when an axis was out of range
    output$ord_caution <- renderUI({
      msg <- msg_axis_clamped(axes(), input$ord_x, input$ord_y)
      if(is.null(msg)) character(0) else tags$p(msg)
    })

    # Plot
    gg <- reactive({
      # settings.  A method does not always return four components, so the
      # axis numbers are brought back into range instead of stopping the panel.
      x <- pick_axis(axes(), input$ord_x)
      y <- pick_axis(axes(), input$ord_y)
      req(x, y)

      if(isTRUE(input$ord_show_group) && has_group(ord_scores(), input$ord_group)){
        alpha <- input$ggplot_alpha
        size  <- input$ggplot_point_size

        gg <- 
          ggplot2::ggplot(ord_scores(), ggplot2::aes(.data[[x]], .data[[y]], label = rownames(ord_scores()))) +
          ggplot2::geom_point(ggplot2::aes(col = .data[[input$ord_group]]), alpha = alpha, size = size) +
          ggplot2::geom_text() +
          ggplot2::theme_bw()
      } else {
        gg <- 
          ggplot2::ggplot(ord_scores(), ggplot2::aes(.data[[x]], .data[[y]], label = rownames(ord_scores()))) +
          ggplot2::geom_text() + 
          ggplot2::theme_bw()
      }
      gg
    })

    # Render
    output$ordination <- renderPlot(res = 96, {
      gg()
    })

  })
}
  # devtools::load_all("../ecan/R")
