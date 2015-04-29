#!/bin/sh
dataDir='pct-data'

if [ ! -d ${dataDir} ]; then
  git clone --depth=1 https://github.com/npct/pct-data.git
else
  cd ${dataDir}
  git pull
fi

exit 0

