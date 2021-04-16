
<!-- README.md is generated from README.Rmd. Please edit that file -->

# caladaptr.apps

`caladaptr.apps` provides an easy way to run Shiny apps that use the
[caladpatr](https://ucanr-igis.github.io/caladaptr/) package.

# Installation

``` r
## Install caladaptr (which is a dependent package, not on CRAN)
remotes::install_github("ucanr-igis/caladaptr")

## Install caladaptr.apps and all dependent packages
remotes::install_github("ucanr-igis/caladaptr.apps")
library(caladaptr.apps)
```

# Usage

There is only one function, `ca_launch`. Use this function to launch
apps saved in the package.

``` r
library(caladaptr.apps)

## Launch the time series app
ca_launch("timeseries")

## Launch the projected chill portions app
ca_launch("chill")
```
