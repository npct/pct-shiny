installed <- cranPkgs %in% installed.packages()
# install packages that are missing
if(length(cranPkgs[!installed]) > 0) install.packages(cranPkgs[!installed], repos='https://cran.rstudio.com/')
