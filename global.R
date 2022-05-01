library(vegan)
library(tidyverse)
library(shiny)

data(dune)
data(dune.env)
df_sample <- 
  dune %>%
  tibble::rownames_to_column("stand") %>%
  tidyr::pivot_longer(-!!"stand", names_to = "species", values_to = "cover") %>%
  dplyr::filter(cover != 0) %>%
  dplyr::left_join(tibble::rownames_to_column(dune.env, "stand"))

