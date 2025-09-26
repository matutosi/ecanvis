## Generate example data
gen_example_data <- function(){
  data(dune)
  data(dune.env)
  sp_dummy <- 
    tibble::tibble(
      "species" = colnames(dune), 
      "dummy_1" = stringr::str_sub(colnames(dune), 1, 1),
      "dummy_6" = stringr::str_sub(colnames(dune), 6, 6)
    )
  example <- 
    dune %>%
    table2df(st = "stand", sp = "species", ab = "cover") %>%
    dplyr::left_join(tibble::rownames_to_column(dune.env, "stand")) %>%
    dplyr::left_join(sp_dummy)
  return(example)
}

## UI module
load_dataUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(

        # Instruction
        tags$ol(
          tags$li('Select "Use example data" or "Upload file"'),
          tags$li('Select column to use bigram'),
        ),
        
        # Example or upload file
        fileInput(ns("file"), "upload file"),
        checkboxInput(ns("file_s_jis"), "Encoding: S-JIS (CP932) JP Windows", value = FALSE),
        tags$hr(),
        checkboxInput(ns("use_example"), "Use example data", value = TRUE),
        tags$hr(),

        # stand, species, cover
        htmlOutput(ns("dupulicated_caution")),
        selectInput(ns("st"), "Unit (stand): " ,     choices = character(0)),
        selectInput(ns("sp"), "Item (species): ",    choices = character(0)),
        selectInput(ns("ab"), "Value (abundance): ", choices = character(0)),

        # Downlod example
        tags$hr(),
          downloadButton(ns("dl_example_data"), "Downlaod example data"),
          htmlOutput(ns("download_example")),
        ),

      mainPanel(
        reactableOutput(ns("table")),
      )

    )
  )
}

## Server module
load_dataServer <- function(id, example_data){
  moduleServer(id, function(input, output, session){

    # File name to upload
    uploaded_file <- reactive({
      req(input$file)
      input$file
    })

    data_in <- reactive({
      locale <- if(input$file_s_jis) readr::locale(encoding = "CP932")
        else                         readr::default_locale()
      data_in <- 
        if(input$use_example){
          example_data
        } else {
          req(input$file)
          try(
            readr::read_delim(uploaded_file()$datapath, locale = locale, show_col_types = FALSE)
          )
        }
      if(inherits(data_in, "try-error")) data_in <- tibble::tibble("Select correct file encoding" = "")

      # Return data
      data_in
    })


    # # # Update selectInput # # #
    observeEvent(c(data_in(), input$use_example), {
      req(data_in())
      choices <- colnames(data_in())
      updateSelectInput(session, "st", choices = choices, selected = choices[1])
      updateSelectInput(session, "sp", choices = choices, selected = choices[2])
      updateSelectInput(session, "ab", choices = choices, selected = choices[3])
    })
    # check dupulicated columns
    output$dupulicated_caution <- renderUI({
      req(input$s, input$sp, input$ab)
      if(input$st == input$sp | input$st == input$ab | input$sp == input$ab){
        tags$h3("Unit, item, and abundance should NOT be dupulicated!")
      } else {
        NULL
      }
    })    

    cols <- reactive({
      req(data_in())
  #       req(input$s, input$sp, input$ab)
      c(input$st, input$sp, input$ab)
    })

    # Download example
    output$download_example <- 
      renderUI("Example data is generated with data dune and dune.env in library vegan.")

    filename <- "example_data.csv"
    output$dl_example_data <- downloadHandler(
      filename = filename,
      content  = function(file) { readr::write_csv(example_data, file) }
    )

    # Show table
    output$table <- renderReactable({
      req(data_in(), cols())
      reactable::reactable(dplyr::relocate(data_in(), cols()), resizable = TRUE, filterable = TRUE, searchable = TRUE)
    })

    # Return data
    reactive({
      req(data_in(), cols())
      dplyr::relocate(data_in(), cols())
    })

  })
}
