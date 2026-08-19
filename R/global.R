  # Packages to attach.  The modules call shiny, reactable and magrittr
  # functions without a namespace, so they have to be on the search path.
pkgs <- c("cluster", "dave", "dendextend", "ggdendro", "ggrepel", "graphics",
          "labdsv", "magrittr", "pkgload", "reactable", "rlang", "rmarkdown",
          "shiny", "shinycssloaders", "tidyverse", "vegan")

  # Install what is missing (base packages such as graphics are always there).
  # require() alone does not attach a package it has just installed, so the
  # install and the attach are kept apart.
for(pkg in setdiff(pkgs, rownames(utils::installed.packages()))){
  utils::install.packages(pkg)
}

  # ecan is on GitHub only.  Install it once: force = TRUE on every start
  # re-installed it each time the app was launched.
if(!requireNamespace("ecan", quietly = TRUE)){
  if(!requireNamespace("devtools", quietly = TRUE)) utils::install.packages("devtools")
  devtools::install_github("matutosi/ecan")
}

for(pkg in pkgs) library(pkg, character.only = TRUE)

  # Module files.  shiny sources ui.R and server.R itself.
  # shiny sets the working directory to the app directory (this directory),
  # but fall back to R/ so that the file also works from the project root.
app_dir <- if(file.exists("cluster.R")) "." else "R"
app_files <- c("utils.R", "data_download.R", "load_data.R",
               "diversity.R", "ind_val.R", "cluster.R", "ordination.R")
for(f in app_files) source(file.path(app_dir, f))
