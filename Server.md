Copied from https://oracle.mythic-beasts.com/7592/Summary converted to md

# Summary

Propensity to Cycle is a mapping tool that combines census data with mapping data
to draw pretty pictures of how often people cycle.

You can see the data for Cambridge here,

http://www.pct.bike/cambridgeshire/

## Servers

We have two 4GB SSD based VMs running the tool (London/Cambridge).

```
 npt1.vs.mythic-beasts.com
 npt2.vs.mythic-beasts.com
```

The data is entirely provided by shiny-server, a webserver and framework for the R statistical package which uses WebSockets.  As R is single threaded we run multiple R processes per server and front with a load balancer (haproxy) which balances requests amongst all the backend processes.


## Configuration

It's a Debian Jessie setup, haproxy at the front end with shiny-server at the back end. Systemd runs all the shiny-servers with their configuration in /etc/shiny-server and /etc/systemd/system. The shiny-server-N files are built by script

```
 /usr/local/sbin/deploy-shiny <no-of-shiny-servers>
```

## Deployment

New code and data updates are pushed by git into the user git@npt1.vs.mythic-beasts.com. A post hook kicks off the deployment scripts in /home/git/bin, which shut down the shiny servers, update the code, update the data, start the shiny servers

## Setting up a new host

Shiny-server has a bunch of badly packaged dependencies. Start with a based managed Debian Jessie install and,

Install R, use [CRAN](http://cran.rstudio.com/bin/linux/debian/) with the debian repository.

```
 #Install the maintainers key
apt-key adv --keyserver keyserver.ubuntu.com --recv-key '95C0FAF38DB3CCAD0C080A7BDC78B2DDEABC47B7'

 root@npt1:/etc/apt# tail -2 /etc/apt/sources.list
 # CRAN R statistical packages
 deb http://cloud.r-project.org/bin/linux/debian buster-cran35
```

Install base R

```
 apt-get update
 apt-get install r-recommended
```

Pull the npt stuff from git

```
 apt-get install git
 su git
 git clone --bare https://github.com/npct/pct-shiny.git pct-shiny
 git clone --bare https://github.com/npct/pct-outputs-regional-Rsmall.git pct-outputs-regional-Rsmall
 cd pct-outputs-regional-Rsmall
 # Set up so checking out the data git dir uses a different work tree location
 # This stops us copying a .git directory around on the server
 git config --bool core.bare false
 git config core.worktree /home/git/var-shiny/pct-outputs-regional-Rsmall/
 mkdir -p /home/git/var-shiny/pct-outputs-regional-Rsmall/
 exit
```

Install shiny server

```
 R -e "install.packages('shiny', repos='https://cran.rstudio.com/')"
```

Now shiny server, https://www.rstudio.com/products/shiny/download-server/

```
 apt-get install gdebi-core
 wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.17.973-amd64.deb
 dpkg -i shiny-server-1.5.17.973-amd64.deb
```

This gives you a working shiny server on port 3838
 http://npt1.vs.mythic-beasts.com:3838/

We need more R packages, these can be installed directly from R with the install.packages command (similar to CPAN but without that much sensible)

First the dependencies

```
 apt-get install libssl libgdal-dev gdal-bin libproj-dev
```

Install the R packages,

```
 R -e "install.packages(c("shiny", "rgdal", "rgeos", "leaflet", "shinyjs", "digest"), repos = 'https://cran.rstudio.com/')"
```

Now configure it to serve npt

```
 mkdir /var/shiny
 chown -R shiny:shiny /var/shiny
 su git
 git --git-dir /home/git/pct-shiny --work-tree /var/shiny/pct-shiny checkout
 git -C /home/git/pct-regions-outputs-R --work-tree /var/shiny/pct-regional-outputs-R checkout
 exit
```

Running multiple copies


R is single threaded so to improve speed we run <number-of-servers> instances per server
(default is 2 as we are more memory constrained than CPU constrained).

The configuration files are inside the git repo and should be copied by
`shiny-control` on deployment and are inside the `pct-shiny/scripts/` folder. `shiny-control` copies them:

```
/etc/shiny-server/shiny-server.conf
/etc/shiny-server/shiny-server.service
```

`deploy-shiny-server` <number-of-servers> takes the base files below and munges them to fill
in the per-server constants so we can run multiple servers in parallel.
It then does a shutdown and restart of all the shiny servers.

We now need to load balance across them with haproxy. We need sticky sessions as shiny-server stores per client state.

```
apt-get install haproxy
scp root@npt1.vs.mythic-beasts.com:/etc/haproxy/haproxy.cfg /etc/haproxy/
```

the core of the session config is,

```
 frontend http-in
        bind ipv4@:80
        bind ipv6@:80
        option httplog
        option forwardfor
        default_backend shiny_server

 backend shiny_server
        balance roundrobin
        cookie SRV_ID insert indirect nocache
        server hex_3838 46.235.225.97:3838 maxconn 5000 check cookie hex_3838
        server hex_3839 46.235.225.97:3839 maxconn 5000 check cookie hex_3839
        server cam_3838 46.235.225.97:3838 maxconn 5000 check cookie cam_3838
        server cam_3839 46.235.225.97:3839 maxconn 5000 check cookie cam_3839
```

... we've configured this for 32 processes on each host in production so you don't need to reconfigure haproxy unless you exceed 32 shiny servers per VM

## Final test
 
The shiny webserver shows a static page at the home page, to really be interacting with R (and shiny) we need to go to

http://npct0.vs.mythic-beasts.com/m/?r=isle-of-wight on the test server and http://npt1.vs.mythic-beasts.com/m/?r=isle-of-wight http://npt2.vs.mythic-beasts.com/m/?r=isle-of-wight on production 
 
## Unused services

As mail and NFS RPC services are not needed

```
apt remove nfs-kernel-server nfs-common rpcbind
apt remove exim4
/etc/init.d/exim4 stop
```

## SSL

We use lets encrypt, in the crontab of npt1:
```
42 3 * * 0,2,4 /usr/local/sbin/le-renew-haproxy >> /var/log/le-renewal.log 2>&1
```
where `le-renew-haproxy` is in this repository: `scripts/le-renew-haproxy`.
On npt2 in `.ssh/authorized_keys` we have the npt1 ha renewal key prefixed with:
```
command="/usr/local/sbin/rrsync /etc/haproxy/certs",no-port-forwarding,no-x11-forwarding,no-agent-forwarding
```
and we simply need to reload haproxy to pick up the new key so in the crontab we have
```
51 3 * * 0,2,4 /usr/sbin/service haproxy reload >> /var/log/le-renewal.log
```

## Deployment of Updates

We've scripted deployment of updates by the git user.  In /pct-shiny/hooks/post-update
```
#!/bin/bash

git update-server-info
git --work-tree /home/git/pct-shiny --git-dir /home/git/pct-shiny show production:scripts/publish-pct-shiny > /home/git/bin/publish-pct-shiny
git --work-tree /home/git/pct-shiny --git-dir /home/git/pct-shiny show production:scripts/update-server > /home/git/bin/update-server
/home/git/bin/publish-pct-shiny
```

which extracts scripts from the pct-shiny repo into

```
bin/publish-pct-shiny
bin/update-server [fully-qualified-server-url]
```

publish-pct-shiny: uses git to build the full copy of files required in /home/git/var-shiny

update-server: shuts down all the shiny servers on the host, rsyncs /home/git/var-shiny into the right place, starts up the shiny servers

You need to makes sure the supporting scripts are installed on each box that the git user is pushing updates to (see /home/git/bin/publish-pct-shiny for the list of servers it will deploy to)

```
 cd /usr/local/sbin/
 scp root@npt1.vs.mythic-beasts.com:/usr/local/sbin/rrsync .
 scp root@npt1.vs.mythic-beasts.com:/usr/local/sbin/shiny-control .
```

and you also need the ssh authorized_keys config for the keys

```
command="/usr/local/sbin/shiny-control $SSH_ORIGINAL_COMMAND",no-port-forwarding,no-x11-forwarding,no-agent-forwarding ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDuzCaVH7xoSzZtNshjOJBQc2RQSwT1jwcXkLg7RtQUHJI42NTtM5jewDdWChkyUAoZKzZMfBh9IndjEdqirYoGHAcujSfZBvarwrlYmhtS+Ip3lfDybyzMUsUquHO7vw08dZByyT3Nl7Ner7Tcn1CBhgDSl8w1NDdtTz81eLyIPTMlx7v9XLcwG5XcixR1iVsk3tFoz7Mgig1CR0cfmAhKOq92Dlfoe7RHGyXjo+CiTQ+VLFL+PgwpIGG+CkDlCSqpN4lDxgcVZbPF0jzZHD1SvNPKVALCuLZDWSdGh4XyENObDLk5YThjY3yudIA49t0VcViQM3uV6+jEwqBBJ2E9 git@npt1
command="/usr/local/sbin/rrsync /var/shiny",no-port-forwarding,no-x11-forwarding,no-agent-forwarding ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCrk5fjbsfLf+63YuJ1tNRlHsiFF9X5F6RQdLtxeyB18xJnxUjLW82fwqRGIwGpYH0i2yTX941hEq2NFmvfsqCgZMK+H7zkLlBulXTo5CShVbGnuZg3/ZrSDKR9QlpgYKH06R8LCEwi83FpT2Uyq1zdBVwIDFLexUVwOi4MqMfatN7xTgDYBECbcpJS2pJonkkwRWXGVKu8KYw1uyotUd3elBVKiwNzeMIiLGPP9bx3yQL4mfwTtAaxr9YWRUAE26dkD5yrt/HH2bPYrsTxlbJm79Cd5zwJjXjJRbdurk2h8pWOqyALopAuL1xrTeoyaYlXnwBWcDJYLyjcTjydanYN git@npt1
```

Also make sure the user can set `LANG` so comment out
```
AcceptEnv LANG LC_*
```
in `/etc/ssh/sshd_config`

## ICT

This is a separate shiny application that lives in the same server. Clone the ICT git into /home/git/ICT on the server with the git user


```sh
git clone https://github.com/ITHIM/ICT.git ICT
```
add a /home/git/bin/publish-ict script

and a post-update hook in the ICT repo

```sh
#!/bin/bash

git update-server-info
/home/git/bin/publish-ict
```
Update all the shiny config files to include

```
location /ICT/ {
  app_dir /var/shiny/ICT/app;
}
```
The dependencies are:

```
apt-get install libssh2-1-dev
```
and

```
R -e "install.packages(c('DT', 'devtools', 'shinyBS', 'shinyjs', 'dplyr', 'plyr', 'stringr', 'Hmisc'), repos='https://cran.rstudio.com/')"
R -e "devtools::install_github('rCharts', 'ramnathv')"
```
