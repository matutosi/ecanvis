if(!require("ecan")) devtools::install_github("matutosi/ecan")

library(ecan)
library(vegan)
library(tidyverse)
library(shiny)

  # generate sample data for download
data(dune)
data(dune.env)
sample_data <-
  dune %>%
  table2df(st = "stand", sp = "species", ab = "cover") %>%
  dplyr::left_join(tibble::rownames_to_column(dune.env, "stand"))

  # ui module (sample)
histPlotUI <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        sliderInput(ns("bins"), "Number of bins: ", min = 1, max = 50, value = 30),
      ),
      mainPanel(
        plotOutput(ns("plot"))
      )
    )
  )
}
  # server module (sample)
histPlot <- function(input, output, session){
  output$plot <- renderPlot({
    x <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins)
  })
}


  # ui module for cluster
clusterPlotUI <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        # method
        selectInput(ns("cl_c_method"), "clustering method",
          choices = c("average", "ward.D", "ward.D2", "single",
                      "complete", "mcquitty", "median", "centroid", "diana")
        ),
        selectInput(ns("cl_d_method"), "distance method",
          choices = c("bray", "euclidean", "correlation", "manhattan",
                      "canberra", "clark", "kulczynski", "jaccard",
                      "gower", "altGower", "morisita", "horn",
                      "mountford", "raup", "binomial", "chao", "cao",
                      "mahalanobis", "chisq", "chord", "aitchison",
                      "robust.aitchison")
        ),
        # stand or species
        checkboxInput(ns("st_or_sp"), "check when cluster with species", value = FALSE),
      ),
      mainPanel(
        plotOutput(ns("clustering")),
      )
    )
  )
}
  # server module for cluster
clusterPlot <- function(input, output, session){
  output$clustering <- renderPlot({
    res <- try(silent = TRUE,
      if(!is.null(data_file())){
        cls <- 
          data_file() %>%
          df2table(st = as.character(input$st), 
                   sp = as.character(input$sp), 
                   ab = as.character(input$ab)) %>%
          t_if_true(input$st_or_sp) %>% # t() when chekcbox selected
          clustering(c_method = input$cl_c_method, d_method = input$cl_d_method)
        ggdendro::ggdendrogram(cls)
      }
    )
    if(length(res) == 1) if(class(res) == "try-error") res <- NULL
    res
  })
}
