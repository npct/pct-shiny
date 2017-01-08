#!/bin/bash
# Install shiny-server and dependencies for the PCT
# Building on Server.md

GIT_DIR=${1##*/}
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# CRAN R statistical packages
gpg --keyserver pgp.mit.edu --recv-key 51716619E084DAB9
gpg -a --export 51716619E084DAB9 > cran.asc
sudo apt-key add cran.asc
rm cran.asc
echo "deb http://cran.rstudio.com/bin/linux/ubuntu xenial/" | sudo tee -a /etc/apt/sources.list
sudo apt-get update
sudo apt-get -yy install r-recommended git gdebi-core

# Install shiny
R -e "install.packages('shiny', repos = 'https://cran.rstudio.com/')"

wget https://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.5.1.834-amd64.deb
sudo gdebi shiny-server-1.5.1.834-amd64.deb
sudo apt-get -yy install libcurl4-openssl-dev libgdal-dev gdal-bin libproj-dev libgeos-dev

sudo R -e "install.packages(c('shiny', 'rgdal', 'rgeos', 'leaflet', 'DT', 'shinyjs', 'dplyr', 'readr'), repos='https://cran.rstudio.com/')"

sudo mkdir /var/shiny
sudo chown -R shiny:shiny /var/shiny

cd ${HOME}
git clone --bare $1

cp -f ${SCRIPT_DIR}/generate-server/post-update ${GIT_DIR}/hooks
sudo cp -f ${SCRIPT_DIR}/generate-server/shiny-server.conf /etc/shiny-server/shiny-server.conf
sudo systemctl restart shiny-server
