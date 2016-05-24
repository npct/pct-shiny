initDevEnv <- function(dataDirRoot, data_sha, cranPkgs, shinyRoot) {
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

  warning("Checking out a the version of the data saved in the data sha")

  # Comment the following line to stop auto checkout
  system2("git", c("--git-dir", file.path(dataDirRoot, ".git"), "--work-tree",
                   dataDirRoot, "checkout", data_sha), wait=T)

  gitArgs <- c("rev-parse", "--short", "HEAD", ">", file.path(shinyRoot, "repo_sha"))

  # Use shell command for Windows as it's failing with system2 for Windows (giving status 128)
  if (.Platform$OS.type == "windows"){
    shell(paste("git", gitArgs, collapse = ' '), wait = T)
  } else {
    system2("git", gitArgs, wait = T)
  }
}