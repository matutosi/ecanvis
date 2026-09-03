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
ordinationSever <- function(id, data_in, com_table){
  moduleServer(id, function(input, output, session){

    st <- reactive({ colnames(data_in)[1] })
    sp <- reactive({ colnames(data_in)[2] })
    ab <- reactive({ colnames(data_in)[3] })

    # Update group select
    indiv <- eventReactive(c(input$ord_show_group, input$ord_use_species_scores), {
      indiv <- pick_indiv(input$ord_use_species_scores, st(), sp())
      if(isTRUE(input$ord_show_group)){
        choices <- ecan::cols_one2multi(data_in, indiv, include_self = FALSE)
        updateSelectInput(session, "ord_group", choices = choices)
      }
      indiv
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
        if(isTRUE(input$ord_show_group) && has_group(data_in, input$ord_group)){
          ecan::ord_add_group(
            ord    = ord(), 
            score  = score(),
            df     = data_in,
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
