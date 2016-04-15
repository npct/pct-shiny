# Clone the data repo if it do not exist
if(!dir.exists(dataDirRoot)) {
  system2('git', args=c('clone', '--depth=1',
                        'https://github.com/npct/pct-data.git', dataDirRoot))
} else { # Update the data repo
  print("Fetching data")
  system2('git', c("--git-dir", file.path(dataDirRoot, ".git"), "--work-tree",
                   dataDirRoot, "fetch"), wait = F)
}

installed <- cranPkgs %in% installed.packages()
# install packages that are missing
if(length(cranPkgs[!installed]) > 0) install.packages(cranPkgs[!installed], repos='https://cran.rstudio.com/')

# Delete the data_sha file to stop auto checkout, e.g. with:
# file.remove("data_sha")
data_sha_file <- file.path(shinyRoot, "data_sha")
if (file.exists(data_sha_file)){
  data_sha <- readLines(data_sha_file)
  warning("Checking out a the version of the data saved in the data sha")
  system2("git", c("--git-dir", file.path(dataDirRoot, ".git"), "--work-tree",
                   dataDirRoot, "checkout", data_sha), wait=T)
}
system2("git", c("rev-parse", "--short", "HEAD", ">", file.path(shinyRoot, "repo_sha")), wait=T)
