# clone the data repo if it do not exist
# issue in ShinyServer that if
# includeHTML("pathTo/file.html")
# and "pathTo/file.html" does not exist then ShinyServer will error
# before running any code
# TL;DR in a newly clone repo this line needs to be run manually
#if(!dir.exists(dataDirRoot)) system2('git', args=c('clone', '--depth=1', 'https://github.com/npct/pct-data.git', dataDirRoot))

# Download files
setwd(dataDirRoot)
system2('git', args=c("pull"), wait = FALSE)
setwd(file.path('..', 'master'))
