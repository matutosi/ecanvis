  # https://matutosi.shinyapps.io/ecanvis/
if(!require("ecan"))            devtools::install_github("matutosi/ecan")
if(!require("ggdendro"))        install.packages("ggdendro")
if(!require("reactable"))       install.packages("reactable")
if(!require("shinycssloaders")) install.packages("shinycssloaders")

library(ecan)
library(vegan)
library(tidyverse)
library(shiny)
  # runApp("d:/matu/work/todo/ecanvis")

instructions <- 
  tags$ol(
    tags$h3('Instruction'),
    tags$li('Select "Use sample data" or "Upload file"'), 
    tags$li('Show data'),
    tags$li('Specify vars as inputs')
  )
