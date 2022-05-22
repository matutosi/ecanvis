  # https://matutosi.shinyapps.io/ecanvis/
function(input, output, session){

  # # # Input data # # #
  data_in <- load_fileSever("load_file")

  # stand, species, cover
  #   output$st <- renderUI({varSelectInput("st", "Unit (stand): " ,     data = data_in(), selected = colnames(data_in())[1]) })
  #   output$sp <- renderUI({varSelectInput("sp", "Item (species): ",    data = data_in(), selected = colnames(data_in())[2]) })
  #   output$ab <- renderUI({varSelectInput("ab", "Value (abandance): ", data = data_in(), selected = colnames(data_in())[3]) })
  observeEvent(data_in(), {
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


  # # # List of data # # #
  all_data <- eventReactive(c(data_in, input$st, input$sp, input$ab), {
print("all_data")
print(input$st)
    list(
      data_in   = data_in(),
      com_table = com_table(),
      st        = as.character(input$st),
      sp        = as.character(input$sp),
      ab        = as.character(input$ab),
      cols      = cols()
    )
  })
  #   all_data <- reactive({
  #     list(
  #       data_in   = data_in(),
  #       com_table = com_table(),
  #       st        = as.character(input$st),
  #       sp        = as.character(input$sp),
  #       ab        = as.character(input$ab),
  #       cols      = cols()
  #     )
  #   })


  # # # Diversity # # #
  diversity <- reactive({
    req(data_in)
    calculate_diversity(data_in, input$st, input$sp, input$ab)
  })
  #   diversity <- calculate_diversity(all_data())

  output$diversity_table <- renderReactable({
    reactable::reactable(diversity(), resizable = TRUE, filterable = TRUE, searchable = TRUE,)
  })

  diversitySever("diversity", diversity())


  # # # Indicator Species Analysis # # #
  #   ind_valSever("ind_val_1", all_data())


  # # # Clusters # # #
  #   clusterSever("cls_1", com_table())
  #   clusterSever("cls_2", com_table())
  #   clusterSever("cls_3", com_table())
  clusterSever("cls_4", com_table())

  # # # Ordinations # # #
  output$ord_note <- renderUI('When error, choose correct "Scores for plot", "Use Group" and "Select group".')
  #   ordinationSever("ord_1", all_data())
  #   ordinationSever("ord_2", all_data())
  #   ordinationSever("ord_3", all_data())
  ordinationSever("ord_4", all_data())
}
