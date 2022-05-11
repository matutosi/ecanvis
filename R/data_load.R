data_loadInput <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), "Upload file", accept = c(".csv", ".tsv", ".txt")),
    checkboxInput(ns("file_s_jis"), "Encoding: S-JIS (CP932) JP Windows", value = FALSE),
  )
}

data_loadServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({ 
      locale <- if(input$file_s_jis) locale(encoding = "CP932") else default_locale()
      req(input$file)
      readr::read_delim(input$file$datapath, locale = locale, show_col_types = FALSE)
    })
  })
}
