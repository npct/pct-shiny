# pct-shiny

Interactive map for prioritising funding for cycling.

### Setup

To run the code in this repository you will need a few things, primarily
a working version of R. Using RStudio will make your life easier.
Running the following code should make the code reproducible on most
computers:

```r
pkgs <- c("shiny", "dplyr", "devtools")
install.packages(pkgs)
lapply(pkgs, library, character.only = T)
# Install leaflet package from github
install_github("rstudio/leaflet")
```

### Set the CYCLESTREET Env variable

Some of the examples pull data from the
[CycleStreets.net API](http://www.cyclestreets.net/api/).
Once you have a token, you can add it to R as follows:

```R
Sys.setenv(CYCLESTREET = "my_token")
```

or in Ubuntu can be added as a session wide var
```bash
echo "export CYCLESTREET='my_token'" >> ~/.profile
```
or system wide
```bash
sudo echo "export CYCLESTREET='my_token'" > /etc/profile.d/cyclestreet.sh
```

### To install shiny-server

* build from source https://github.com/rstudio/shiny-server/wiki/Building-Shiny-Server-from-Source
* add cyclestreet token to ENV
* use default server config `wget https://raw.githubusercontent.com/rstudio/shiny-server/master/config/default.config > /etc/shiny-server/shiny-server.conf`
* use default upstart `wget  https://raw.github.com/rstudio/shiny-server/master/config/upstart/shiny-server.conf  -O /etc/init/shiny-server.conf`

### Deploying

We use a branch called 'production' for the deployed version:

```bash
git push production master
```
