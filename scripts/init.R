init_dev_env <- function(interface_root, data_regional_root, outputs_regional_sha, cranPkgs) {

  ## Clone the regional data repo if it does not exist
  if(!dir.exists(data_regional_root)) {
    system2('git', args=c('clone', '--depth=3',
                          'https://github.com/npct/pct-outputs-regional.git', data_regional_root))
  } else { # Update the data repo
    print("Fetching regional data")
    system2('git', c("--git-dir", file.path(data_regional_root, ".git"), "--work-tree",
                     data_regional_root, "fetch"), wait = F)
  }

  ## Check out the data in regional sha
  ## Comment the following lines to stop auto checkout
  # warning("Checking out the version of the data saved in the regional sha")
  # sys_message = c("--git-dir", file.path(data_regional_root, ".git"),
  #                 "--work-tree", data_regional_root, "checkout", outputs_regional_sha)
  # system2("git", sys_message, wait=T)

  ## Get the current repo sha
  gitArgs <- c("rev-parse", "--short", "HEAD", ">", file.path(interface_root, "repo_sha"))
  # Use shell command for Windows as it's failing with system2 for Windows (giving status 128)
  if (.Platform$OS.type == "windows"){
    shell(paste(append("git", gitArgs), collapse = " "), wait = T)
  } else {
    system2("git", gitArgs, wait = T)
  }

  ## Install packages
  installed <- cranPkgs %in% installed.packages()
  # install packages that are missing
  if(length(cranPkgs[!installed]) > 0) install.packages(cranPkgs[!installed], repos='https://cran.rstudio.com/')

}