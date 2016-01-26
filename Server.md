## Shiny on the server
### Installing shiny-server

* build from source https://github.com/rstudio/shiny-server/wiki/Building-Shiny-Server-from-Source
* add cyclestreet token to ENV
* use default server config `wget https://raw.githubusercontent.com/rstudio/shiny-server/master/config/default.config > /etc/shiny-server/shiny-server.conf`
* use default upstart `wget  https://raw.github.com/rstudio/shiny-server/master/config/upstart/shiny-server.conf  -O /etc/init/shiny-server.conf`

### Data files

The data files are kept in a seaprate repository https://github.com/npct/pct-data.git
The script update-data.sh pulls the latest version of the data.  This will timeout on a Shiny server so it is
advised to add

```bash
0,30 * * * * /path_to_shiny_repo/pct-shiny/scrips/update-data.sh
```
to the crontab (`crontab -e`)

### Git repo

To create a new repo that users can push to

```bash
mkdir repo_name
cd repo_name
git init --shared=group
#only make repo bare after creating .git folder
git config --bool core.bare true

#set hook to be run when repo receives a push
echo "#\!/bin/sh \nunset GIT_INDEX_FILE\ngit --work-tree=`pwd` --git-dir=`pwd`/.git checkout -f" > .git/hooks/post-update
chmod +x .git/hooks/post-update

chgrp -R users .
chmod -R g+rwX .

# set directory to have group write AND setgid so new objects inherit permissions
find . -type d -exec chmod g+s '{}' +
find . -type f -exec chmod ug+rw {} \;
```
