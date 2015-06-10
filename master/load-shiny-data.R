# script for downloading and saving data for pct-shiny
# run if data is not up-to-date

# windows specific version
if (Sys.info()["sysname"] != "Windows") {
  # Download data files
  # This will timeout on the server (so a cron job is used instead)
  # but will work locally
  old <- setwd('..')
  system2(file.path('master', 'update-data.sh'), wait = FALSE)
  setwd(old)
}else {
  # clone the data repo if it do not exist
  if(!dir.exists(data_dir)){
    # Download files
    old <- setwd(data_dir)
    system2('git', args=c("pull"), wait = FALSE)
    setwd(old)
  } else
    print("No pct-data folder in pct-shiny")
  }

