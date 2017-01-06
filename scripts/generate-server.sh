#!/bin/bash
# Install shiny-server and dependencies for the PCT
# Inspired by github.com/keberwein/Data_Science_Workbench
# Building on Server.md

 #Install the maintainers key
 apt-key adv --keyserver keys.gnupg.net --recv-key 381BA480

 # CRAN R statistical packages
echo "deb http://cran.rstudio.com/bin/linux/ubuntu xenial/" | tee -a /etc/apt/sources.list
gpg --keyserver pgp.mit.edu --recv-key 51716619E084DAB9
gpg -a --export 51716619E084DAB9 > cran.asc
apt-key add cran.asc
rm cran.asc
apt-get -yy install r-recommended git gdebi-core

# Install shiny
 R -e "install.packages('shiny', repos = 'https://cran.rstudio.com/')"

wget https://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.5.1.834-amd64.deb
gdebi shiny-server-1.5.1.834-amd64.deb