To update static pages make edits to files in this folder, then run the following commands (requires Ruby to be installed):

```sh
# gem update --system # You might need this if you have an old version and bundle install fails
gem install bundler:2.1.4
cd jekyll-static
bundle install
bundle exec jekyll s -d ../regions_www
```
