## UI module
data_download_tsvUI <- function(id, label = "Download file") {
  ns <- NS(id)
  tagList(
    downloadButton(ns("data_download_tsv"), "Download tsv data"),
  )
}

## Server module
data_download_tsvSever <- function(id, data, filename){
  moduleServer(id, function(input, output, session){
    output$data_download_tsv = downloadHandler(
      filename = paste0(filename, ".tsv"),
      content  = function(file) { readr::write_tsv(data, file) }
    )
  })
}
