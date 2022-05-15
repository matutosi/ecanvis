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

        # stand, species, cover, stand group, species group, 
        uiOutput("st"),
        uiOutput("sp"),
        uiOutput("ab"),
        uiOutput("use_st_gr"),
        uiOutput("st_gr"),
        uiOutput("use_sp_gr"),
        uiOutput("sp_gr"),

    tags$hr(),
        htmlOutput("download_example"),
        downloadButton("dl_example_data", "Downlaod example data"),
      ),

      mainPanel(
        reactableOutput("table"),
      )
    )
  ),

  # # # Clustering (Dynamic) # # #
  # IN PROGRESS (not work as expected)
  #   tabPanel("Cluster",
  #     numericInput("no_cls", "No. of clustering ( < 6 )",
  #                  value = 2, min = 1, max = 5, step = 1,),
  #     uiOutput("clusters")
  #   ),

  # # # Clusterings # # #
  tabPanel("Clusterings",
    clusterUI("cls_1"),
    clusterUI("cls_2"),
    clusterUI("cls_3"),
    clusterUI("cls_4")
  ),

  # # # Ordinations # # #
  tabPanel("Ordinations",
    ordinationUI("ord_1"),
    ordinationUI("ord_2"),
    ordinationUI("ord_3"),
    ordinationUI("ord_4")
  )
)
