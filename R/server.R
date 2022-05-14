  # https://matutosi.shinyapps.io/ecanvis/
function(input, output, session){

  # # # Input data # # #
  data_in <- load_fileSever("load_file")

  output$st    <- renderUI({varSelectInput("st",    "unit (stand): " ,     data = data_in(), selected = colnames(data_in())[1])})
  output$sp    <- renderUI({varSelectInput("sp",    "item (species): ",    data = data_in(), selected = colnames(data_in())[2]) })
  output$ab    <- renderUI({varSelectInput("ab",    "value (abandance): ", data = data_in(), selected = colnames(data_in())[3]) })

  #   output$st_gr <- renderUI({varSelectInput("st_gr", "unit group (opt): " , data = data_in())})
  #   output$sp_gr <- renderUI({varSelectInput("sp_gr", "item group:(opt): ",  data = data_in())})
  #   output$st    <- select_varServer("test1", data=data_in())
  #   output$sp    <- select_varServer("test2", data=data_in())
  #   output$ab    <- select_varServer("test3", data=data_in())

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
  #   clusterSever("cls_1", data_in(), input$st, input$sp, input$ab)
  #   clusterSever("cls_2", data_in(), input$st, input$sp, input$ab)
  #   clusterSever("cls_3", data_in(), input$st, input$sp, input$ab)
  clusterSever("cls_4", reactive(data_in), reactive(input$st), reactive(input$sp), reactive(input$ab))

  # # # Ordination (comparison) # # #
  ordinationSever("ord_1", data_in(), input$st, input$sp, input$ab)
  ordinationSever("ord_2", data_in(), input$st, input$sp, input$ab)
  ordinationSever("ord_3", data_in(), input$st, input$sp, input$ab)
  ordinationSever("ord_4", data_in(), input$st, input$sp, input$ab)

}
