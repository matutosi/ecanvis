
function(input, output) {

  data_file <- reactive({ 
    if(is_null(input$file)) { return(NULL) }
    else                    { read_delim(input$file$datapath) }
  })

  output$sccater_plot <- renderPlot({
    df_sample %>%
      ggplot(aes(x = !!input$var_1, y = !!input$var_2)) + 
      geom_point() + 
      theme_bw()
  })

  output$table <- renderDataTable({
    data_file()
  })

  #   # For debug
  #   output$table <- renderDataTable({
  #      data_file() %>% dplyr::select(input$st, input$sp, input$ab)
  #   })
  #   output$stand <- renderText(input$st)


  output$clustering <- renderPlot({
    if(!is.null(data_file())){
      data_file() %>%
  # need obserb() ?
        df2table(st = input$st, sp = input$sp, ab = input$ab) %>%
  #         df2table(st = "stand", sp = "species", ab = "cover") %>%
        clustering(c_method = input$cl_c_method, d_method = input$cl_d_method) %>%
        plot()
    }
  })

  output$st <- renderUI({
    varSelectInput("st", "stand: " , data = data_file())
  })

  output$sp <- renderUI({
    varSelectInput("sp", "species: " , data = data_file())
  })

  output$ab <- renderUI({
    varSelectInput("ab", "abandance: " , data = data_file())
  })


}
