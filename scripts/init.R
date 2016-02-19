data_sha <- readLines(file.path(shinyRoot, "data_sha"))

installed <- cranPkgs %in% installed.packages()
# install packages that are missing
if(length(cranPkgs[!installed]) > 0) install.packages(cranPkgs[!installed], repos='https://cran.rstudio.com/')

system2("git", c("--git-dir", file.path(dataDirRoot, ".git"), "--work-tree",
                 dataDirRoot, "checkout", data_sha, "-f"), wait=T)
