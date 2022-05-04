  # https://matutosi.shinyapps.io/ecanvis/
navbarPage("ecan",
  # # # Input data # # #
  tabPanel("Read file",
    sidebarLayout(
      sidebarPanel(
  #         actionButton("use_sample_data", "use sample data"),
        fileInput("file", "choose file"),
        checkboxInput("file_s_jis", "Encoding: S-JIS (CP932) JP Windows", value = FALSE),

        uiOutput("st"),
        uiOutput("sp"),
        uiOutput("ab"),
        uiOutput("st_gr"),
        uiOutput("sp_gr"),

        htmlOutput("please_download"),

        downloadButton("dl_sample_data", "downlaod sample data"),
      ),
      mainPanel(
        dataTableOutput("table"),
      )
    )
  ),

  # # # Clustering # # #
  tabPanel("Clustering",
    clusterUI("cls_1"),
    clusterUI("cls_2"),
    clusterUI("cls_3"),
    clusterUI("cls_4")
  #     sidebarLayout(
  #       sidebarPanel(
  #         # method
  #         selectInput("cl_c_method", "clustering method",
  #           choices = c("average", "ward.D", "ward.D2", "single",
  #                       "complete", "mcquitty", "median", "centroid", "diana")
  #         ),
  #         selectInput("cl_d_method", "distance method",
  #           choices = c("bray", "euclidean", "correlation", "manhattan",
  #                       "canberra", "clark", "kulczynski", "jaccard",
  #                       "gower", "altGower", "morisita", "horn",
  #                       "mountford", "raup", "binomial", "chao", "cao",
  #                       "mahalanobis", "chisq", "chord", "aitchison",
  #                       "robust.aitchison")
  #         ),
  #         #
  #         checkboxInput("st_or_sp", "check when cluster with species", value = FALSE),
  #       ),
  #       mainPanel(
  #         plotOutput("clustering"),
  #       )
  #     )
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
        plotOutput("ordination"),
      )
    )
  ),

  tabPanel("hist",
    histUI("bins1"),
    histUI("bins2")
  )
)
