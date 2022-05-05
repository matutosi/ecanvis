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

        htmlOutput("download_sample"),

        downloadButton("dl_sample_data", "Downlaod sample data"),
      ),
      mainPanel(
        dataTableOutput("table"),
      )
    )
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
    HTML(r'(If not working, 
      <ol> 
        <li> reload app 
        <li> read a file 
        <li> specify "unit", "item" and "value" in "Read file" tab 
        <li>  select "Clustering (comparison)" tab.
      </ol>)'),
    clusterUI("cls_1"),
    clusterUI("cls_2"),
    clusterUI("cls_3"),
    clusterUI("cls_4")
  ),

  # # # Ordination (comparison) # # #
  tabPanel("Ordination (comparison)",
    HTML(r'(If not working, 
      <ol> 
        <li> reload app 
        <li> read a file 
        <li> specify "unit", "item" and "value" in "Read file" tab 
        <li>  select "Clustering (comparison)" tab.
      </ol>)'),
    ordinationUI("ord_1"),
    ordinationUI("ord_2"),
    ordinationUI("ord_3"),
    ordinationUI("ord_4")
  ),

)
