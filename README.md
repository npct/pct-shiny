# pct-shiny

Interactive map for prioritising funding for cycling.

### Setup

To run the code in this repository you will need a few things, primarily
a working version of R. Using RStudio will make your life easier.
Running the following code should make the code reproducible on most
computers:

```r
pkgs <- c("shiny", "RColorBrewer", "httr", "rgdal", "rgeos")
install.packages(pkgs)
lapply(pkgs, library, character.only = T)
# Install leaflet package from github
devtools::install_github("rstudio/leaflet")
devtools::install_github("rstudio/DT")
```
