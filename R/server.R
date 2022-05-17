  # https://matutosi.shinyapps.io/ecanvis/
function(input, output, session){

  # # # Input data # # #
  data_in <- load_fileSever("load_file")
  # stand, species, cover
  output$st <- renderUI({varSelectInput("st", "unit (stand): " ,     data = data_in(), selected = colnames(data_in())[1]) })
  output$sp <- renderUI({varSelectInput("sp", "item (species): ",    data = data_in(), selected = colnames(data_in())[2]) })
  output$ab <- renderUI({varSelectInput("ab", "value (abandance): ", data = data_in(), selected = colnames(data_in())[3]) })

  # stand group
  #   use_select_varSever("st", data = data_in(), group_label = "unit group", selected_col = 6) # "st-gr")
  # species group
  #   id_sp <- "st"
  #   use_select_varSever(id_sp, data = data_in(), group_label = "item group", selected_col = 1) # NS(id_st, "gr")


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


  # # # Diversity # # #
  diversity <- 
    calculate_diversity(data_in(), input$st, input$sp, input$ab)

  output$diversity_table <- renderReactable({
    reactable::reactable(diversity(), resizable = TRUE, filterable = TRUE, searchable = TRUE,)
  })

  diversitySever("diversity", diversity())
  #   diversitySever("diversity", diversity(), input$"st-gr")


  # # # Clustering (Dynamic) # # #
  # IN PROGRESS (not work as expected)
  #   output$clusters <- renderUI({
  #     no_cls <- paste0("cls_", seq_len(input$no_cls))
  #     print(no_cls) # for debug
  #     map(no_cls, clusterUI)
  #     map(no_cls, clusterSever, com_table)
  #   })

  # # # Clusterings # # #
  #   clusterSever("cls_1", com_table)
  #   clusterSever("cls_2", com_table)
  #   clusterSever("cls_3", com_table)
  clusterSever("cls_4", com_table)

  # # # Ordinations # # #
  #   ordinationSever("ord_1", com_table)
  #   ordinationSever("ord_2", com_table)
  #   ordinationSever("ord_3", com_table)
  ordinationSever("ord_4", com_table)

}
