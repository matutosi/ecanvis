  # https://matutosi.shinyapps.io/ecanvis/
function(input, output, session){

  # # # Input data # # #
  data_in <- load_fileSever("load_file")

  output$st    <- renderUI({varSelectInput("st",    "unit (stand): " ,     data = data_in(), selected = colnames(data_in())[1])})
  output$sp    <- renderUI({varSelectInput("sp",    "item (species): ",    data = data_in(), selected = colnames(data_in())[2]) })
  output$ab    <- renderUI({varSelectInput("ab",    "value (abandance): ", data = data_in(), selected = colnames(data_in())[3]) })
  output$st_gr <- renderUI({varSelectInput("st_gr", "unit group (opt): " , data = data_in())})
  output$sp_gr <- renderUI({varSelectInput("sp_gr", "item group:(opt): ",  data = data_in())})

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

  # # # Clustering (Dynamic) # # #
  # IN PROGRESS (not work as expected)
  #   output$clusters <- renderUI({
  #     no_cls <- paste0("cls_", seq_len(input$no_cls))
  #     print(no_cls) # for debug
  #     map(no_cls, clusterUI)
  #     map(no_cls, clusterSever, com_table)
  #   })

  # # # Clusterings # # #
  clusterSever("cls_1", com_table)
  clusterSever("cls_2", com_table)
  clusterSever("cls_3", com_table)
  clusterSever("cls_4", com_table)

  # # # Ordinations # # #
  ordinationSever("ord_1", com_table)
  ordinationSever("ord_2", com_table)
  ordinationSever("ord_3", com_table)
  ordinationSever("ord_4", com_table)

}
