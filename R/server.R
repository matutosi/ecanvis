  # https://matutosi.shinyapps.io/ecanvis/
function(input, output, session){

  # # # Input data # # #
  data_in <- load_fileSever("load_file")

  # stand, species, cover
  observeEvent(data_in(), {
    req(data_in())
    choices <- colnames(data_in())
    updateSelectInput(session, "st", choices = choices, selected = choices[1])
    updateSelectInput(session, "sp", choices = choices, selected = choices[2])
    updateSelectInput(session, "ab", choices = choices, selected = choices[3])
  })


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
    req(data_in())
    df2table(data_in(),
             st = as.character(input$st),
             sp = as.character(input$sp),
             ab = as.character(input$ab))
  })

  # # # Colnames except c(st, sp, ab)  # # #
  cols <- reactive({
    st_sp_ab <- c(as.character(input$st), as.character(input$sp), as.character(input$ab))
    setdiff(colnames(data_in()), st_sp_ab)
  })



  # # # Diversity # # #
  observeEvent(c(data_in(), input$st, input$sp, input$ab), ignoreInit = TRUE, {
    diversitySever("diversity", data_in(), input$st, input$sp, input$ab)
  })


  # # # Indicator Species Analysis # # #
  #   ind_valSever("ind_val_1", all_data())


  # # # Clusters # # #
  observeEvent(c(data_in(), input$st, input$sp), ignoreInit = TRUE, {
    clusterSever("cls_1", data_in(), input$st, input$sp, com_table())
    clusterSever("cls_2", data_in(), input$st, input$sp, com_table())
    clusterSever("cls_3", data_in(), input$st, input$sp, com_table())
    clusterSever("cls_4", data_in(), input$st, input$sp, com_table())
  })

  # # # Ordinations # # #
  observeEvent(c(data_in(), input$st, input$sp), ignoreInit = TRUE, {
    ordinationSever("ord_1", data_in(), input$st, input$sp, com_table())
    ordinationSever("ord_2", data_in(), input$st, input$sp, com_table())
    ordinationSever("ord_3", data_in(), input$st, input$sp, com_table())
    ordinationSever("ord_4", data_in(), input$st, input$sp, com_table())
  })

}

  # devtools::load_all("../ecan/R")
