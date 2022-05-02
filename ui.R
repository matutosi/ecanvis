  # fluidPage(
navbarPage("ecan",
  #   titlePanel("Sample data (dune and dune.env from vegan)"),

  tabPanel("Sample data",
    sidebarLayout(
      sidebarPanel(
        varSelectInput("var_1", "x: ", df_sample),
        varSelectInput("var_2", "y: ", df_sample),
      ),
      mainPanel(
        plotOutput("sccater_plot"),
      )
    )
  ),

  tabPanel("Read file",
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "choose file"),

        uiOutput("st"), 
        uiOutput("sp"), 
        uiOutput("ab"), 

      ),
      mainPanel(
        dataTableOutput("table")
      )
    )
  ),

  tabPanel("Clustering",
    sidebarLayout(
      sidebarPanel(

        selectInput("cl_c_method", "clustering method",
          choices = c("average", "ward.D", "ward.D2", "single",
                      "complete", "mcquitty", "median", "centroid", "diana")
        ),
        selectInput("cl_d_method", "distance method",
          choices = c("bray", "euclidean", "correlation", "manhattan",
                      "canberra", "clark", "kulczynski", "jaccard",
                      "gower", "altGower", "morisita", "horn",
                      "mountford", "raup", "binomial", "chao", "cao",
                      "mahalanobis", "chisq", "chord", "aitchison",
                      "robust.aitchison")
        ),
      ),
      mainPanel(
        plotOutput("clustering"),
      )
    )
  ),

)
