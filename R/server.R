  # https://matutosi.shinyapps.io/ecanvis/
function(input, output, session){


  # # # Load data # # #
  data_in <- load_dataServer("read_data", example_data = gen_example_data())

  # # # Community table # # #
  com_table <- reactive({
    req(data_in())
    df2table(data_in(), st = colnames(data_in())[1],
                        sp = colnames(data_in())[2],
                        ab = colnames(data_in())[3])
  })

  # # # Diversity # # #
  #   observe({
  #     diversitySever("diversity", data_in())
  #   })
  observeEvent(data_in(), {
    diversitySever("diversity", data_in())
  })


  # # # Indicator Species Analysis # # #
  observeEvent(c(data_in(), input$st, input$sp, input$ab), ignoreInit = TRUE, {
    ind_valSever("ind_val", data_in(), input$st, input$sp, input$ab)
  })

  # # # Clusters # # #
  observeEvent(c(data_in(), input$st, input$sp), ignoreInit = TRUE, {
    clusterSever("cls_1", data_in(), input$st, input$sp, com_table())
    clusterSever("cls_2", data_in(), input$st, input$sp, com_table())
    clusterSever("cls_3", data_in(), input$st, input$sp, com_table())
    clusterSever("cls_4", data_in(), input$st, input$sp, com_table())
  })

  # # # Ordinations # # #
  observeEvent(c(data_in(), input$st, input$sp), ignoreInit = TRUE, {
    ordinationSever("ord_1", data_in(), input$st, input$sp, input$ab, com_table())
    ordinationSever("ord_2", data_in(), input$st, input$sp, input$ab, com_table())
    ordinationSever("ord_3", data_in(), input$st, input$sp, input$ab, com_table())
    ordinationSever("ord_4", data_in(), input$st, input$sp, input$ab, com_table())
  })

}
  # devtools::load_all("../ecan/R")
