#!/bin/bash
# Install shiny-server and dependencies for the PCT
# Building on Server.md

GIT_DIR=${1##*/}

# CRAN R statistical packages
gpg --keyserver pgp.mit.edu --recv-key 51716619E084DAB9
gpg -a --export 51716619E084DAB9 > cran.asc
apt-key add cran.asc
rm cran.asc
echo "deb http://cran.rstudio.com/bin/linux/ubuntu xenial/" | tee -a /etc/apt/sources.list
apt-get -yy install r-recommended git gdebi-core

# Install shiny
 R -e "install.packages('shiny', repos = 'https://cran.rstudio.com/')"

wget https://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.5.1.834-amd64.deb
gdebi shiny-server-1.5.1.834-amd64.deb
apt-get -yy install libssl libgdal-dev gdal-bin libproj-dev

R -e "install.packages(c('shiny', 'rgdal', 'rgeos', 'leaflet', 'DT', 'shinyjs', 'dplyr', 'readr'), repos='https://cran.rstudio.com/')"

mkdir /var/shiny
sudo chown -R shiny:shiny /var/shiny

git clone --bare $1

cp -f generate-server/post-update ${GIT_DIR}/hooks
sudo cp -f generate-server/shiny-server.conf /etc/shiny-server/shiny-server.conf
