## UI module
data_download_tsvUI <- function(id, label = "Download file") {
  ns <- NS(id)
  tagList(
    downloadButton(ns("data_download_tsv"), "Download tsv data"),
  )
}

## Server module
##   data and filename may be plain values or reactives; they are forced
##   inside the handlers so that a download always reflects the current state.
data_download_tsvServer <- function(id, data, filename){
  moduleServer(id, function(input, output, session){
    output$data_download_tsv = downloadHandler(
      filename = function() { paste0(as_value(filename), ".tsv") },
      content  = function(file) { readr::write_tsv(as_value(data), file) }
    )
  })
}
