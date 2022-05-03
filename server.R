function(input, output) {

  # # # Sample data # # # 
  #   output$sccater_plot <- renderPlot({
  #     df_sample %>%
  #       ggplot(aes(x = !!input$var_1, y = !!input$var_2)) + 
  #       geom_point() + 
  #       theme_bw()
  #   })

  # # # Input data # # # 
  data_file <- reactive({ 
    if(is_null(input$file)) { return(NULL) }
    else                    { readr::read_delim(input$file$datapath) }
  })

  output$table <- renderDataTable({
    data_file()
  })

  output$st    <- renderUI({ varSelectInput("st",    "unit (stand): " ,     data = data_file()) })
  output$sp    <- renderUI({ varSelectInput("sp",    "item (species): ",    data = data_file()) })
  output$ab    <- renderUI({ varSelectInput("ab",    "value (abandance): ", data = data_file()) })
  #   output$st_gr <- renderUI({ varSelectInput("st_gr", "unit group (opt): " , data = data_file()) })
  #   output$sp_gr <- renderUI({ varSelectInput("sp_gr", "item group:(opt): ",  data = data_file()) })

  # sample data
  #   observeEvent(input$use_sample_data, {
  #     readr::read_delim("df_sample.tsv")
  #   })
  output$please_download <- renderUI("First time to use, please download a sample file and upload it, The sample file is generated with data dune and dune.env in library vegan.")

  output$dl_sample_data = downloadHandler(
    filename = "sample_data.tsv",
    content  = function(file) { readr::write_tsv(sample_data, file) }
  )

  # # # Clustering # # # 
  output$clustering <- renderPlot({
    if(!is.null(data_file())){
      cls <- 
        data_file() %>%
        df2table(st = as.character(input$st), 
                 sp = as.character(input$sp), 
                 ab = as.character(input$ab)) %>%
        t_if_true(input$st_or_sp) %>% # t() when chekcbox selected
        clustering(c_method = input$cl_c_method, d_method = input$cl_d_method)
        ggdendro::ggdendrogram(cls)
    }
  })

  # # # Ordination # # # 
  output$ordination <- renderPlot({
    if(!is.null(data_file())){
      ord <- 
        data_file() %>%
        df2table(st = as.character(input$st), 
                 sp = as.character(input$sp), 
                 ab = as.character(input$ab)) %>%
        ordination(o_method = input$or_o_method, d_method = input$or_d_method)
        ord_plot(ord, score = input$or_score, x = input$or_x,  y = input$or_y)
    }
  })

}
