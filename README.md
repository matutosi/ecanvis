
# ecanvis

The goal of ecanvis is to support ecological analysis and its
visualization with shiny.

## How to use

Run on web (shinyapps.io).

<https://matutosi.shinyapps.io/ecanvis/>

Run on your local PC.

``` r
  # Install packages (need only once)
if(!require("shiny"))     install.packages("shiny")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("devtools"))  install.packages("devtools")
if(!require("ggdendro"))  install.packages("ggdendro")
if(!require("ecan"))      devtools::install_github("matutosi/ecan")

  # Run app
shiny::runGitHub("matutosi/ecanvis", subdir = "R")
```

## Citation

Toshikazu Matsumura (2021) Morphological analysis for Japanese with R
and shiny. <https://matutosi.shinyapps.io/ecanvis/>.

# Make your shiny app

Home: <https://shiny.rstudio.com/>

Gallery: <https://shiny.rstudio.com/gallery/>

Tutorial: <https://shiny.rstudio.com/tutorial/>

shinyapps.io: <https://www.shinyapps.io/>

Book: <https://mastering-shiny.org/>

Book (in Japanese): <https://www.amazon.co.jp/dp/4863542577/>
