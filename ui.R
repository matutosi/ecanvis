fluidPage(
titlePanel("Sample data (dune and dune.env from vegan)"),

  sidebarLayout(
    sidebarPanel(
      
      varSelectInput("var_1", "x: ", df_sample),
      varSelectInput("var_2", "y: ", df_sample),

  #       sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30),
  #       selectInput("color", "select color", c("red", "blue", "green", "black"))
    ),


    mainPanel(
  #       plotOutput("distPlot"),
      plotOutput("sccater_plot")

    )
  )
)
