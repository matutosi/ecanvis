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

      tags$hr(),
        htmlOutput("download_example"),
        downloadButton("dl_example_data", "Downlaod example data"),
      ),

      mainPanel(
        reactableOutput("table"),
      )
    )
  ),

  tabPanel("Diversity",
    diversityUI("diversity"),
    reactableOutput("diversity_table")
  ),

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
