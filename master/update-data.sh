#!/bin/sh
# This is run via cron job on the server,
# NOTE: it uses the relative path so the cron user must have the pct-shiny dir:
# /home/shinyUser/pct-shiny
# and this wil create
# /home/shinyUser/pct-data
# with a crontab of
# 23,53 * * * * /home/shinyUser/pct-shiny/master/update-data.sh
dataDir='pct-data'

if [ ! -d ${dataDir} ]; then
  git clone --depth=1 https://github.com/npct/pct-data.git
else
  cd ${dataDir}
  git pull
fi

exit 0

