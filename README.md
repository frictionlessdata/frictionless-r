
<!-- README.md is generated from README.Rmd. Please edit that file -->

# frictionless

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/frictionless)](https://CRAN.R-project.org/package=frictionless)
[![R-CMD-check](https://github.com/frictionlessdata/frictionless-r/workflows/R-CMD-check/badge.svg)](https://github.com/frictionlessdata/frictionless-r/actions)
[![codecov](https://codecov.io/gh/frictionlessdata/frictionless-r/branch/main/graph/badge.svg?token=bKtiHW21K0)](https://codecov.io/gh/frictionlessdata/frictionless-r)
[![repo
status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Funding](https://img.shields.io/static/v1?label=powered+by&message=lifewatch.be&labelColor=1a4e8a&color=f15922)](https://lifewatch.be/)
<!-- badges: end -->

Frictionless is an R package to read and write Frictionless Data
Packages. A [Data
Package](https://specs.frictionlessdata.io/data-package/) is a simple
container format to describe and package a collection of (tabular) data.
It is typically used to publish FAIR and open datasets.

To get started, see:

-   [Get
    started](https://frictionlessdata.github.io/frictionless-r/articles/frictionless.html):
    an introduction to the package’s main functionality.
-   [Function
    reference](https://frictionlessdata.github.io/frictionless-r/reference/index.html):
    overview of all functions.

## Installation

You can install the development version from
[GitHub](https://github.com/frictionlessdata/frictionless-r) with:

``` r
# install.packages("devtools")
devtools::install_github("frictionlessdata/frictionless-r")
```

## Usage

With frictionless you can directly load data from a Data Package (local
or remote) into your R environment. Here we load bird GPS tracking data
from a Data Package published on
[Zenodo](https://doi.org/10.5281/zenodo.5070086):

``` r
library(frictionless)

# Read the datapackage.json file
# This gives you access to all Data Resources of the Data Package
# without reading them, which is convenient and fast.
package <- read_package("https://zenodo.org/record/5070086/files/datapackage.json")
#> Please make sure you have the right to access data from this Data Package for your intended use.
#> Follow applicable norms or requirements to credit the dataset and its authors.
#> For more information, see https://doi.org/10.5281/zenodo.5070086

# List the available resources
package$resource_names
#> [1] "reference-data" "gps"            "acceleration"

# Read data from the resource "gps"
# This will return a single data frame, even though the data are 
# split over multiple CSV files.
read_resource(package, "gps")
#> # A tibble: 73,047 × 21
#>     `event-id` visible timestamp           `location-long` `location-lat`
#>          <dbl> <lgl>   <dttm>                        <dbl>          <dbl>
#>  1 14256075762 TRUE    2018-05-25 16:11:37            4.25           51.3
#>  2 14256075763 TRUE    2018-05-25 16:16:41            4.25           51.3
#>  3 14256075764 TRUE    2018-05-25 16:21:29            4.25           51.3
#>  4 14256075765 TRUE    2018-05-25 16:26:28            4.25           51.3
#>  5 14256075766 TRUE    2018-05-25 16:31:21            4.25           51.3
#>  6 14256075767 TRUE    2018-05-25 16:36:09            4.25           51.3
#>  7 14256075768 TRUE    2018-05-25 16:40:57            4.25           51.3
#>  8 14256075769 TRUE    2018-05-25 16:45:55            4.25           51.3
#>  9 14256075770 TRUE    2018-05-25 16:50:49            4.25           51.3
#> 10 14256075771 TRUE    2018-05-25 16:55:36            4.25           51.3
#> # … with 73,037 more rows, and 16 more variables:
#> #   bar:barometric-pressure <dbl>, external-temperature <dbl>, gps:dop <dbl>,
#> #   gps:satellite-count <dbl>, gps-time-to-fix <dbl>, ground-speed <dbl>,
#> #   heading <dbl>, height-above-msl <dbl>, location-error-numerical <dbl>,
#> #   manually-marked-outlier <lgl>, vertical-error-numerical <dbl>,
#> #   sensor-type <chr>, individual-taxon-canonical-name <chr>,
#> #   tag-local-identifier <chr>, individual-local-identifier <chr>, …
```

You can also create your own Data Package, add data and write it to
disk:

``` r
# Create a data frame (from the built-in dataset iris)
df <- iris

# Create a Data Package and add iris as a resource
my_package <-
  create_package() |>
  add_resource("iris", df)

# Write the Data Package to disk
my_package |>
  write_package(tempdir())
```

For more functionality, see [get
started](https://frictionlessdata.github.io/frictionless-r/articles/frictionless.html)
or the [function
reference](https://frictionlessdata.github.io/frictionless-r/reference/index.html).

## frictionless vs datapackage.r

[datapackage.r](https://cran.r-project.org/web/packages/datapackage.r/)
is an alternative R package to work with Data Packages. It has an
object-oriented design (using a `Package` class) and offers validation.
frictionless on the other hand allows users to quickly read and write
Data Package data to and from R data frames, getting out of your way for
the rest of your analysis. It is designed to be lightweight, follows
[tidyverse](https://www.tidyverse.org/) principles and supports piping.

## Meta

-   We welcome
    [contributions](https://frictionlessdata.github.io/frictionless-r/CONTRIBUTING.html)
    including bug reports.
-   License: MIT
-   Get [citation
    information](https://frictionlessdata.github.io/frictionless-r/authors.html#citation)
    for frictionless in R doing `citation("frictionless")`.
-   Please note that this project is released with a [Contributor Code
    of
    Conduct](https://frictionlessdata.io/work-with-us/code-of-conduct/).
    By participating in this project you agree to abide by its terms.
