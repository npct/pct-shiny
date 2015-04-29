# pct-shiny

Interactive map for prioritising funding for cycling.

### Setup

To run the code in this repository you will need a few things, primarily
a working version of R. Using RStudio will make your life easier.
Running the following code should make the code reproducible on most
computers:

```r
pkgs <- c("shiny", "shinyBS", "RColorBrewer", "httr", "rgdal", "downloader", "rgeos", "curl", "jsonlite")
install.packages(pkgs)
lapply(pkgs, library, character.only = T)
# Install leaflet package from github
install_github("rstudio/leaflet")
```

### To install shiny-server

* build from source https://github.com/rstudio/shiny-server/wiki/Building-Shiny-Server-from-Source
* add cyclestreet token to ENV
* use default server config `wget https://raw.githubusercontent.com/rstudio/shiny-server/master/config/default.config > /etc/shiny-server/shiny-server.conf`
* use default upstart `wget  https://raw.github.com/rstudio/shiny-server/master/config/upstart/shiny-server.conf  -O /etc/init/shiny-server.conf`

### Data files

The data files are kept in a seaprate repository https://github.com/npct/pct-data.git
The script update-data.sh pulls the latest version of the data.  This will timeout on a Shiny server so it is
advised to add

```bash
0,30 * * * * /path-to-shiny-repo/pct-shiny/update-data.sh
```

to the crontab (`crontab -e`)
