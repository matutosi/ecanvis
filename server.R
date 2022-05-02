function(input, output) {

  output$sccater_plot <- renderPlot({
    df_sample %>%
      ggplot(aes(x = !!input$var_1, y = !!input$var_2)) + 
      geom_point() + 
      theme_bw()
  })


  output$table <- renderDataTable({
    df <- read_delim(input$file$datapath)
    df
  })

  output$clustering <- renderPlot({
    df <- read_delim(input$file$datapath)
    tbl <- df2table(df, st = "stand", sp = "species", ab = "cover")
    cl <- clustering(tbl, c_method = input$cl_c_method, d_method = input$cl_d_method)
    plot(cl)
  })


}
