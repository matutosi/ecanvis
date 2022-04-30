library(vegan)
library(tidyverse)
library(shiny)

data(dune)
data(dune.env)
df_sample <- 
  dune %>%
  tibble::rownames_to_column("st") %>%
  tidyr::pivot_longer(-st, names_to = "species", values_to = "cover") %>%
  dplyr::left_join(tibble::rownames_to_column(dune.env, "stand"))

