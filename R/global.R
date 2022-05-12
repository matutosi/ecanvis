  # https://matutosi.shinyapps.io/ecanvis/
if(!require("ecan"))            devtools::install_github("matutosi/ecan")
if(!require("ggdendro"))        install.packages("ggdendro")
if(!require("reactable"))       install.packages("reactable")
if(!require("shinycssloaders")) install.packages("shinycssloaders")

library(ecan)
library(vegan)
library(tidyverse)
library(shiny)

source("load_data.R")

source("data_load.R")
source("cluster.R")
source("data_sample.R")
source("ordination.R")
source("select_var.R")
