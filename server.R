function(input, output) {

  output$sccater_plot <- renderPlot({
    df_sample %>%
      ggplot(aes(x = !!input$var_1, y = !!input$var_2)) + 
      geom_point() + 
      theme_bw()
  })


  output$table <- renderDataTable({
    read_delim(input$file$datapath)
  })


  output$clustering <- renderPlot({
    input$cl_c_method
    input$cl_d_method

  })

  #   bins <- reactive({
  #     x = faithful[, 2]
  #     return (seq(min(x), max(x), length.out = input$bins + 1))
  #   })
  # 
  #   output$distPlot <- renderPlot({
  #     hist(faithful[, 2], breaks = bins(), col =  input$color, border = 'white')
  #   })

}
