  # https://matutosi.shinyapps.io/ecanvis/
navbarPage("ecan",

  # # # Read data # # #
  tabPanel("Read data 2",
    load_dataUI("read_data")
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
