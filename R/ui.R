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
        selectInput("st", "Unit (stand): " ,     choices = character(0)),
        selectInput("sp", "Item (species): ",    choices = character(0)),
        selectInput("ab", "Value (abandance): ", choices = character(0)),

      tags$hr(),
        htmlOutput("download_example"),
        downloadButton("dl_example_data", "Downlaod example data"),
      ),

      mainPanel(
        reactableOutput("table"),
      )
    )
  ),


  # # # Diversity # # #
  tabPanel("Diversity",
    diversityUI("diversity")
  ),

  # # # Indicator Species Analysis # # #
  tabPanel("ISA (Ind val)",
    ind_valUI("ind_val")
  ),

  # # # Clusters # # #
  tabPanel("Clusters",
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
