  # https://matutosi.shinyapps.io/ecanvis/
if(!require("ecan"))            devtools::install_github("matutosi/ecan")
if(!require("ggdendro"))        install.packages("ggdendro")
if(!require("reactable"))       install.packages("reactable")
if(!require("shinycssloaders")) install.packages("shinycssloaders")

library(ecan)
library(vegan)
library(tidyverse)
library(shiny)
library(zeallot)

source("data_load.R")
source("data_example.R")

source("diversity.R")

source("cluster.R")
source("ordination.R")
