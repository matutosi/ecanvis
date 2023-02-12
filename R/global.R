  # https://matutosi.shinyapps.io/ecanvis/
if(!require("devtools"))        install.packages("devtools")
  # if(!require("ecan"))            devtools::install_github("matutosi/ecan")
                                devtools::install_github("matutosi/ecan", force = TRUE)
if(!require("vegan"))           install.packages("vegan")
if(!require("cluster"))         install.packages("cluster")
if(!require("dave"))            install.packages("dave")
if(!require("dendextend"))      install.packages("dendextend")
if(!require("ggdendro"))        install.packages("ggdendro")
if(!require("graphics"))        install.packages("graphics")
if(!require("labdsv"))          install.packages("labdsv")
if(!require("magrittr"))        install.packages("magrittr")
if(!require("pkgload"))         install.packages("pkgload")
if(!require("reactable"))       install.packages("reactable")
if(!require("rmarkdown"))       install.packages("rmarkdown")
if(!require("shiny"))           install.packages("shiny")
if(!require("shinycssloaders")) install.packages("shinycssloaders")
if(!require("tidyverse"))       install.packages("tidyverse")
if(!require("rlang"))           install.packages("rlang")
if(!require("ggrepel"))         install.packages("ggrepel")
if(!require("zeallot"))         install.packages("zeallot")

library(ecan)
library(vegan)
library(tidyverse)
library(shiny)
library(zeallot)
library(dendextend)

source("load_data.R")
source("data_download.R")
source("diversity.R")
source("ind_val.R")
source("cluster.R")
source("ordination.R")

shiny::enableBookmarking(store = "url")
  # Japanese font settings: usefull in shiny.io
  #   https://github.com/ltl-manabi/shinyapps.io_japanese_font
  # IPAex font
download.file("https://raw.githubusercontent.com/ltl-manabi/shinyapps.io_japanese_font/master/use_ipaex_font.sh", 
  destfile = "use_ipaex_font.sh")
system("bash ./use_ipaex_font.sh")
