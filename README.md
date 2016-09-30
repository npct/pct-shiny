# pct-shiny

Interactive map for prioritising funding for cycling.

### Setup

To run the code in this repository you will need a few things, primarily
a working version of R. Using RStudio will make your life easier.

To start:

```sh
git clone git@github.com:npct/pct-shiny.git
git clone git@github.com:npct/pct-data.git --depth=1
```

so that [pct-data](https://github.com/npct/pct-data) is in a sibling folder.

To create and modify `pct-data` refer to the [pct-load](https://github.com/npct/pct-load), cloned with::

```sh
git clone git@github.com:npct/pct-load.git
```

The `regions_www` folder contains all the current starting regions
which can be run as shiny apps.

See [npct/pct/analysis](https://github.com/npct/pct/blob/master/analysis/mapgen.R) to see how the regional map was generated.
