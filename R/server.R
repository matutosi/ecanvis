  # https://matutosi.shinyapps.io/ecanvis/
function(input, output, session){

  # # # Load data # # #
  data_in <- load_dataServer("read_data", example_data = gen_example_data())

  # # # Community table # # #
  com_table <- reactive({
    req(data_in())
    ecan::df2table(data_in(), st = colnames(data_in())[1],
                        sp = colnames(data_in())[2],
                        ab = colnames(data_in())[3])
  })

  # # # Diversity # # #
  observeEvent(data_in(), {
    diversitySever("diversity", data_in())
  })

  # # # Indicator Species Analysis # # #
  observeEvent(data_in(), {
    ind_valSever("ind_val", data_in())
  })

  # # # Groups shared between the panels # # #
  # The cluster panels put the TWINSPAN they made in here, and the ordination
  # panels offer them as a group, so that the same grouping can be seen on a
  # dendrogram and on an ordination without running TWINSPAN twice.
  tw_store <- reactiveValues()

  # # # Clusters # # #
  observeEvent(c(data_in()), {
    clusterSever("cls_1", data_in(), com_table(), tw_store)
    clusterSever("cls_2", data_in(), com_table(), tw_store)
    clusterSever("cls_3", data_in(), com_table(), tw_store)
    clusterSever("cls_4", data_in(), com_table(), tw_store)
  })

  # # # Ordinations # # #
  observeEvent(c(data_in()), {
    ordinationSever("ord_1", data_in(), com_table(), tw_store)
    ordinationSever("ord_2", data_in(), com_table(), tw_store)
    ordinationSever("ord_3", data_in(), com_table(), tw_store)
    ordinationSever("ord_4", data_in(), com_table(), tw_store)
  })

}
  # devtools::install_github("matutosi/ecan")
  # devtools::load_all("../ecan/R")
