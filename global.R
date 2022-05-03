if(!require("ecan")) devtools::install_github("matutosi/ecan")

library(ecan)
library(vegan)
library(tidyverse)
library(shiny)

  # data(dune)
  # data(dune.env)
  # df_sample <-
  #   dune %>%
  #   table2df(st = "stand", sp = "species", ab = "cover") %>%
  #   dplyr::left_join(tibble::rownames_to_column(dune.env, "stand"))
