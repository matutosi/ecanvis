  # Source the app files that contain function definitions only.
  # global.R, ui.R and server.R hold top level expressions and are skipped.
suppressMessages(library(magrittr))

app_dir <- normalizePath(file.path("..", "..", "R"), mustWork = TRUE)
app_files <- c("utils.R", "data_download.R", "load_data.R",
               "diversity.R", "ind_val.R", "cluster.R", "ordination.R")
for(f in app_files) source(file.path(app_dir, f))
