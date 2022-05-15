  # https://matutosi.shinyapps.io/ecanvis/
function(input, output, session){

  # # # Input data # # #
  data_in <- load_fileSever("load_file")

  output$st    <- renderUI({varSelectInput("st",    "unit (stand): " ,     data = data_in(), selected = colnames(data_in())[1])})
  output$sp    <- renderUI({varSelectInput("sp",    "item (species): ",    data = data_in(), selected = colnames(data_in())[2]) })
  output$ab    <- renderUI({varSelectInput("ab",    "value (abandance): ", data = data_in(), selected = colnames(data_in())[3]) })
  #   output$st_gr <- renderUI({varSelectInput("st_gr", "unit group (opt): " , data = data_in())})
  #   output$sp_gr <- renderUI({varSelectInput("sp_gr", "item group:(opt): ",  data = data_in())})

  # Download example
  output$download_example <-
    renderUI("Example data is generated with data dune and dune.env in library vegan.")
  output$dl_example_data = downloadHandler(
    filename = "example_data.tsv",
    content  = function(file) { readr::write_tsv(gen_example_data(), file) }
  )

  # package reactable: https://glin.github.io/reactable/index.html
  output$table <- renderReactable({
    reactable::reactable(data_in(), resizable = TRUE, filterable = TRUE, searchable = TRUE,)
  })

  # # # Community table # # #
  com_table <- reactive({
    df2table(data_in(), 
             st = as.character(input$st),
             sp = as.character(input$sp),
             ab = as.character(input$ab))
  })


  # editing now
  output$clusters <- renderUI({
    no_cls <- paste0("cls_", seq_len(input$no_cls))
  #     print(no_cls)   # for debug
    print(no_cls) # for debug
    map(no_cls, clusterSever, com_table)
    map(no_cls, clusterUI)
  })



  # # # Clustering # # #
  output$clustering <- renderPlot(res = 96, {
    cls <-
      com_table() %>%
      # stand or species
      t_if_true(input$st_or_sp) %>% # t() when chekcbox selected
      clustering(c_method = input$cl_c_method, d_method = input$cl_d_method)
    ggdendro::ggdendrogram(cls)
  })

  # # # Ordination # # #
  output$ordination <- renderPlot(res = 96, {
    ord <-
      com_table() %>%
      ordination(o_method = input$or_o_method, d_method = input$or_d_method)
    ord_plot(ord, score = input$or_score, x = input$or_x,  y = input$or_y)
  })

  # # # Clustering (comparison) # # #
  #   clusterSever("cls_1", com_table)
  #   clusterSever("cls_2", com_table)
  #   clusterSever("cls_3", com_table)
  #   clusterSever("cls_4", com_table)

  # # # Ordination (comparison) # # #
  ordinationSever("ord_1", com_table)
  ordinationSever("ord_2", com_table)
  ordinationSever("ord_3", com_table)
  ordinationSever("ord_4", com_table)

}
