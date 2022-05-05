  # https://matutosi.shinyapps.io/ecanvis/
if(!require("ecan"))            devtools::install_github("matutosi/ecan")
if(!require("ggdendro"))        install.packages("ggdendro")
if(!require("shinycssloaders")) install.packages("shinycssloaders")

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

  # ui module for cluster
clusterUI <- function(id){
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
        checkboxInput(ns("st_or_sp"), "clustering with species", value = FALSE)
      ),
      mainPanel(
        shinycssloaders::withSpinner(type = sample(1:8, 1), color.background = "white",
          plotOutput(ns("clustering"))
        )
      )
    )
  )
}
  # server module for cluster
clusterSever <- function(id, df, st, sp, ab){
  moduleServer(id, function(input, output, session){
    output$clustering <- renderPlot({
      res <- try(silent = TRUE,
        if(!is.null(df)){
          cls <- 
            df %>%
            df2table(st = as.character(st), 
                     sp = as.character(sp), 
                     ab = as.character(ab)) %>%
            t_if_true(input$st_or_sp) %>% # t() when chekcbox selected
            clustering(c_method = input$cl_c_method, d_method = input$cl_d_method)
          ggdendro::ggdendrogram(cls)
        }
      )
      if(length(res) == 1) if(class(res) == "try-error") res <- NULL
      res
    })
  })
}

  # ui module for ordination
ordinationUI <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        # method
        selectInput(ns("or_o_method"), "ordination method",
          choices = c("pca", "ca", "dca",
                      "pcoa", "fspa", "nmds")
        ),
        selectInput(ns("or_d_method"), "distance method",
          choices = c("bray", "euclidean", "correlation", "manhattan",
                      "canberra", "clark", "kulczynski", "jaccard",
                      "gower", "altGower", "morisita", "horn",
                      "mountford", "raup", "binomial", "chao", "cao",
                      "mahalanobis", "chisq", "chord", "aitchison",
                      "robust.aitchison")
        ),
        # stand or species, x and y axis
        radioButtons(ns("or_score"), "scores for plot",
          choiceNames  = c("unit (stand)", "item (species)"),
          choiceValues = c("st_scores",    "sp_scores")
  #           choiceNames  = c("unit (stand)", "item (species)", "both"),
  #           choiceValues = c("st_scores",    "sp_scores",      "both")
        ),
        numericInput(ns("or_x"), "x axis component",
          value = 1, min = 1, max = 10, step = 1,
        ),
        numericInput(ns("or_y"), "y axis component",
          value = 2, min = 1, max = 10, step = 1,
        ),
      ),
      mainPanel(
        shinycssloaders::withSpinner(type = sample(1:8, 1), color.background = "white",
          plotOutput(ns("ordination"))
        )
      )
    )
  )
}
  # server module for ordination
ordinationSever <- function(id, df, st, sp, ab){
  moduleServer(id, function(input, output, session){
    output$ordination <- renderPlot({
      res <- try(silent = TRUE,
        if(!is.null(df)){
          ord <- 
            df %>%
            df2table(st = as.character(st), 
                     sp = as.character(sp), 
                     ab = as.character(ab)) %>%
            ordination(o_method = input$or_o_method, d_method = input$or_d_method)
          ord_plot(ord, score = input$or_score, x = input$or_x,  y = input$or_y)
        }
      )
      if(length(res) == 1) if(class(res) == "try-error") res <- NULL
      res
    })
  })
}
