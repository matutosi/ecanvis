  # UI module
load_fileInput <- function(id, label = "Upload file") {
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), label),
    checkboxInput(ns("file_s_jis"), "Encoding: S-JIS (CP932) JP Windows", value = FALSE),
    tags$hr(),
    checkboxInput(ns("use_example"), "Use example data", value = TRUE)
  )
}

  # Server module
load_fileSever <- function(id){
  moduleServer(id, function(input, output, session){
    uploaded_file <- reactive({
      req(input$file)
      input$file
    })

    reactive({
      locale <- 
        if(input$file_s_jis) {
          readr::locale(encoding = "CP932")
        } else {
          readr::default_locale()
        }
      if(input$use_example){
         gen_example_data() # global.R
      } else {
        readr::read_delim(uploaded_file()$datapath, locale = locale, show_col_types = FALSE)
      }
    })
  })
}
