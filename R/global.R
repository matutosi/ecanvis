  # Packages the app calls without a namespace: shiny, in every module, and
  # magrittr for %>%.  Everything else is called as pkg::fun and only has to
  # be installed.
  #
  # The names below are written out one by one on purpose.  rsconnect reads
  # the code to decide what to bundle for shinyapps.io, and it only sees a
  # package named in library(), in pkg::fun or in data(package = ).  The list
  # used to be held in a variable and attached in a loop, which made every one
  # of them invisible: the deployed app would then start without them.
  #
  # dave was dropped: it is archived on CRAN and ecan no longer uses it
  # ("fspa" was removed from ordination() in ecan 0.2.1).

  # Install what is missing.  On shinyapps.io the library is built when the app
  # is deployed and cannot be written to, so nothing is installed there: a
  # missing package has to stop the app loudly instead.
if(file.access(.libPaths()[1], mode = 2) == 0){
  pkgs <- c("dendextend", "dplyr", "ggplot2", "ggrepel", "magrittr",
            "reactable", "readr", "shiny", "shinycssloaders", "stringr",
            "tibble", "vegan")
    # require() alone does not attach a package it has just installed, so the
    # install and the attach are kept apart.
  for(pkg in setdiff(pkgs, rownames(utils::installed.packages()))){
    utils::install.packages(pkg)
  }

    # ecan is on GitHub only.  Install it once: force = TRUE on every start
    # re-installed it each time the app was launched.
  if(!requireNamespace("ecan", quietly = TRUE)){
    if(!requireNamespace("remotes", quietly = TRUE)) utils::install.packages("remotes")
    remotes::install_github("matutosi/ecan")
  }
}

library(shiny)
library(magrittr)

  # Module files.  shiny sources ui.R and server.R itself.
  # shiny sets the working directory to the app directory (this directory),
  # but fall back to R/ so that the file also works from the project root.
app_dir <- if(file.exists("cluster.R")) "." else "R"
app_files <- c("utils.R", "data_download.R", "load_data.R",
               "diversity.R", "ind_val.R", "cluster.R", "ordination.R")
for(f in app_files) source(file.path(app_dir, f))
