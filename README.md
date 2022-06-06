
# ecanvis

The goal of ecanvis is to support ecological analysis and its
visualization with shiny.

## How to use

Run on web (shinyapps.io).

<https://matutosi.shinyapps.io/ecanvis/>

Run on your local PC.

``` r
  # Install packages (need only once)
if(!require("devtools"))        install.packages("devtools")
                                devtools::install_github("matutosi/ecan", force = TRUE)
if(!require("cluster"))         install.packages("cluster")
if(!require("dave"))            install.packages("dave")
if(!require("dendextend"))      install.packages("dendextend")
if(!require("ggdendro"))        install.packages("ggdendro")
if(!require("graphics"))        install.packages("graphics")
if(!require("labdsv"))          install.packages("labdsv")
if(!require("magrittr"))        install.packages("magrittr")
if(!require("pkgload"))         install.packages("pkgload")
if(!require("reactable"))       install.packages("reactable")
if(!require("rlang"))           install.packages("rlang")
if(!require("rmarkdown"))       install.packages("rmarkdown")
if(!require("shiny"))           install.packages("shiny")
if(!require("shinycssloaders")) install.packages("shinycssloaders")
if(!require("tidyverse"))       install.packages("tidyverse")
if(!require("vegan"))           install.packages("vegan")

  # Run app
shiny::runGitHub("matutosi/ecanvis", subdir = "R")
```

## Citation

Toshikazu Matsumura (2022) Ecological Analysis Visualization tools with
R and shiny. <https://matutosi.shinyapps.io/ecanvis/>.

# Make your shiny app

-   Home: <https://shiny.rstudio.com/>
-   Gallery: <https://shiny.rstudio.com/gallery/>
-   Tutorial: <https://shiny.rstudio.com/tutorial/>
-   shinyapps.io: <https://www.shinyapps.io/>
-   Book: <https://mastering-shiny.org/>
-   Book (in Japanese): <https://www.amazon.co.jp/dp/4863542577/>
