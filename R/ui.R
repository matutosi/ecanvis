  # https://matutosi.shinyapps.io/ecanvis/
navbarPage("ecan",

  # # # Input data # # #
  tabPanel("Read data",
    sidebarLayout(
      sidebarPanel(

        tags$ol(
          tags$li('Select "Use example data" or "Upload file"'),
          tags$li('Specify vars as inputs')
        ),
        load_fileInput("load_file"),

        uiOutput("st"),
        uiOutput("sp"),
        uiOutput("ab"),
  #         uiOutput("st_gr"),
  #         uiOutput("sp_gr"),

    tags$hr(),
        htmlOutput("download_example"),
        downloadButton("dl_example_data", "Downlaod example data"),
      ),

      mainPanel(
        reactableOutput("table"),
      )
    )
  ),


  # editing now
  # # # Clustering (Dynamic) # # #
  tabPanel("Cluster",
    numericInput("no_cls", "No. of clustering ( < 5 )",
                 value = 2, min = 1, max = 5, step = 1,),
    uiOutput("clusters")
  ),


  # # # Clustering # # #
  tabPanel("Clustering",
    sidebarLayout(
      sidebarPanel(
        # method
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
        #
        checkboxInput("st_or_sp", "clustering with species", value = FALSE),
      ),
      mainPanel(
        shinycssloaders::withSpinner(type = sample(1:8, 1), color.background = "white",
          plotOutput("clustering")
        )
      )
    )
  ),

  # # # Ordination # # #
  tabPanel("Ordination",
    sidebarLayout(
      sidebarPanel(
        # method
        selectInput("or_o_method", "ordination method",
          choices = c("pca", "ca", "dca",
                      "pcoa", "fspa", "nmds")
        ),
        selectInput("or_d_method", "distance method",
          choices = c("bray", "euclidean", "correlation", "manhattan",
                      "canberra", "clark", "kulczynski", "jaccard",
                      "gower", "altGower", "morisita", "horn",
                      "mountford", "raup", "binomial", "chao", "cao",
                      "mahalanobis", "chisq", "chord", "aitchison",
                      "robust.aitchison")
        ),
        # stand or species, x and y axis
        radioButtons("or_score", "scores for plot",
          choiceNames  = c("unit (stand)", "item (species)"),
          choiceValues = c("st_scores",    "sp_scores")
  #           choiceNames  = c("unit (stand)", "item (species)", "both"),
  #           choiceValues = c("st_scores",    "sp_scores",      "both")
        ),
        numericInput("or_x", "x axis component",
          value = 1, min = 1, max = 10, step = 1,
        ),
        numericInput("or_y", "y axis component",
          value = 2, min = 1, max = 10, step = 1,
        ),

      ),
      mainPanel(
        shinycssloaders::withSpinner(type = sample(1:8, 1), color.background = "white",
          plotOutput("ordination")
        )
      )
    )
  ),

  # # # Clustering (comparison) # # #
  tabPanel("Clustering (comparison)",
    clusterUI("cls_1"),
    clusterUI("cls_2"),
    clusterUI("cls_3"),
    clusterUI("cls_4")
  ),

  # # # Ordination (comparison) # # #
  tabPanel("Ordination (comparison)",
    ordinationUI("ord_1"),
    ordinationUI("ord_2"),
    ordinationUI("ord_3"),
    ordinationUI("ord_4")
  )
)
