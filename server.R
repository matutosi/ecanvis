  # https://matutosi.shinyapps.io/ecanvis/
function(input, output, session){

  # # # Input data # # # 
  data_file <- reactive({ 
    if(is_null(input$file)) { return(NULL) }
    else                    { 
      locale <- if(input$file_s_jis) locale(encoding = "CP932") else default_locale()
      readr::read_delim(input$file$datapath, locale = locale, show_col_types = FALSE)
    }
  })

  # package reactable: https://glin.github.io/reactable/index.html
  output$table <- renderReactable({
    if(!is_null(data_file()))
      reactable::reactable(data_file(),  resizable = TRUE, filterable = TRUE, searchable = TRUE,)
  })

  output$st    <- renderUI({ varSelectInput("st",    "unit (stand): " ,     data = data_file()) })
  output$sp    <- renderUI({ varSelectInput("sp",    "item (species): ",    data = data_file()) })
  output$ab    <- renderUI({ varSelectInput("ab",    "value (abandance): ", data = data_file()) })
  #   output$st_gr <- renderUI({ varSelectInput("st_gr", "unit group (opt): " , data = data_file()) })
  #   output$sp_gr <- renderUI({ varSelectInput("sp_gr", "item group:(opt): ",  data = data_file()) })

  output$download_sample <- renderUI("First time to use, please download a sample file and upload it, The sample file is generated with data dune and dune.env in library vegan.")

  output$dl_sample_data = downloadHandler(
    filename = "sample_data.tsv",
    content  = function(file) { readr::write_tsv(sample_data, file) }
  )

  # # # Clustering # # # 
  output$clustering <- renderPlot(res = 96, {
    res <- try(silent = TRUE,
      if(!is.null(data_file())){
        cls <- 
          data_file() %>%
          df2table(st = as.character(input$st), 
                   sp = as.character(input$sp), 
                   ab = as.character(input$ab)) %>%
          # stand or species
          t_if_true(input$st_or_sp) %>% # t() when chekcbox selected
          clustering(c_method = input$cl_c_method, d_method = input$cl_d_method)
        ggdendro::ggdendrogram(cls)
      }
    )
    if(length(res) == 1) if(class(res) == "try-error") res <- NULL
    res
  })

  # # # Ordination # # # 
  output$ordination <- renderPlot(res = 96, {
    res <- try(silent = TRUE,
      if(!is.null(data_file())){
        ord <- 
          data_file() %>%
          df2table(st = as.character(input$st), 
                   sp = as.character(input$sp), 
                   ab = as.character(input$ab)) %>%
          ordination(o_method = input$or_o_method, d_method = input$or_d_method)
        ord_plot(ord, score = input$or_score, x = input$or_x,  y = input$or_y)
      }
    )
    if(length(res) == 1) if(class(res) == "try-error") res <- NULL
    res
  })

  # # # Clustering (comparison) # # # 
  clusterSever("cls_1", data_file(), input$st, input$sp, input$ab)
  clusterSever("cls_2", data_file(), input$st, input$sp, input$ab)
  clusterSever("cls_3", data_file(), input$st, input$sp, input$ab)
  clusterSever("cls_4", data_file(), input$st, input$sp, input$ab)

  # # # Ordination (comparison) # # # 
  ordinationSever("ord_1", data_file(), input$st, input$sp, input$ab)
  ordinationSever("ord_2", data_file(), input$st, input$sp, input$ab)
  ordinationSever("ord_3", data_file(), input$st, input$sp, input$ab)
  ordinationSever("ord_4", data_file(), input$st, input$sp, input$ab)

}
