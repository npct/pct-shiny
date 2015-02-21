# pct-shiny
The Shiny map for Local Autorites

### Set the CYCLESTREET Env variable
This can be done in R
```R
Sys.setenv(CYCLESTREET = "my_token")
```

or in Ubuntu can be added as a session wide var
```bash
echo "CYCLESTREET='my_token'" >> ~/.profile
```
or system wide
```bash
sudo echo "CYCLESTREET='my_token'" > /etc/profile.d/cyclestreet.sh
```
