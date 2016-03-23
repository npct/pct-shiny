# Clone the data repo if it do not exist

if(!dir.exists(dataDirRoot)) {
  system2('git', args=c('clone', '--depth=1',
                        'https://github.com/npct/pct-data.git', dataDirRoot))
}

# Update the data repo
if(dir.exists(dataDirRoot)){
  old <- setwd(dataDirRoot)
  system2('git', "fetch", wait = F)
  setwd(old)
}

data_sha <- readLines(file.path(shinyRoot, "data_sha"))

installed <- cranPkgs %in% installed.packages()
# install packages that are missing
if(length(cranPkgs[!installed]) > 0) install.packages(cranPkgs[!installed], repos='https://cran.rstudio.com/')

# The following lines checkout the version of the data saved in data_sha.
# Uncomment to auto checkout pct-data.
# warning("Checking out a the version of the data saved in the data sha")
# system2("git", c("--git-dir", file.path(dataDirRoot, ".git"), "--work-tree",
#                  dataDirRoot, "checkout", data_sha), wait=T)
# system2("git", c("rev-parse", "--short", "HEAD", ">", file.path(shinyRoot, "repo_sha")), wait=T)