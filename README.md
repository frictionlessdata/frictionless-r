
<!-- README.md is generated from README.Rmd. Please edit that file -->

# frictionless

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/frictionless)](https://CRAN.R-project.org/package=frictionless)
[![CRAN
checks](https://badges.cranchecks.info/worst/frictionless.svg)](https://cran.r-project.org/web/checks/check_results_frictionless.html)
[![R-CMD-check](https://github.com/frictionlessdata/frictionless-r/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/frictionlessdata/frictionless-r/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/frictionlessdata/frictionless-r/branch/main/graph/badge.svg)](https://app.codecov.io/gh/frictionlessdata/frictionless-r/)
[![repo
status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![rOpenSci](https://badges.ropensci.org/495_status.svg)](https://github.com/ropensci/software-review/issues/495)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5815355.svg)](https://doi.org/10.5281/zenodo.5815355)
<!-- badges: end -->

Frictionless is an R package to read and write Frictionless Data
Packages. A [Data
Package](https://specs.frictionlessdata.io/data-package/) is a simple
container format and standard to describe and package a collection of
(tabular) data. It is typically used to publish
[FAIR](https://www.go-fair.org/fair-principles/) and open datasets.

To get started, see:

- [Get
  started](https://docs.ropensci.org/frictionless/articles/frictionless.html):
  an introduction to the package’s main functionalities.
- [Function
  reference](https://docs.ropensci.org/frictionless/reference/index.html):
  overview of all functions.
- [Standard
  implementation](https://docs.ropensci.org/frictionless/articles/index.html):
  how frictionless implements the Data Package standard.

**Frictionless currently implements [Data Package
v1](https://specs.frictionlessdata.io/).** Our goal is to support [Data
Package v2](https://datapackage.org/) as well.

## Installation

Install the latest released version from CRAN:

``` r
install.packages("frictionless")
```

Or the development version from
[GitHub](https://github.com/frictionlessdata/frictionless-r) or
[R-universe](https://ropensci.r-universe.dev):

``` r
# install.packages("devtools")
devtools::install_github("frictionlessdata/frictionless-r")

# Or rOpenSci R-universe
install.packages("frictionless", repos = "https://ropensci.r-universe.dev")
```

## Usage

With frictionless you can **read** data from a Data Package (local or
remote) into your R environment. Here we read bird GPS tracking data
from a Data Package published on
[Zenodo](https://doi.org/10.5281/zenodo.10053702):

``` r
library(frictionless)

# Read the datapackage.json file
# This gives you access to all Data Resources of the Data Package without 
# reading them, which is convenient and fast.
package <- read_package("https://zenodo.org/records/10053702/files/datapackage.json")

package
#> A Data Package with 3 resources:
#> • reference-data
#> • gps
#> • acceleration
#> For more information, see <https://doi.org/10.5281/zenodo.10053702>.
#> Use `unclass()` to print the Data Package as a list.

# List resources
resources(package)
#> [1] "reference-data" "gps"            "acceleration"

# Read data from the resource "gps"
# This will return a single data frame, even though the data are split over 
# multiple zipped CSV files.
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
#> # ℹ 73,037 more rows
#> # ℹ 16 more variables: `bar:barometric-pressure` <dbl>,
#> #   `external-temperature` <dbl>, `gps:dop` <dbl>, `gps:satellite-count` <dbl>,
#> #   `gps-time-to-fix` <dbl>, `ground-speed` <dbl>, heading <dbl>,
#> #   `height-above-msl` <dbl>, `location-error-numerical` <dbl>,
#> #   `manually-marked-outlier` <lgl>, `vertical-error-numerical` <dbl>,
#> #   `sensor-type` <chr>, `individual-taxon-canonical-name` <chr>, …
```

You can also create your own Data Package, add data and **write** it to
disk:

``` r
# Create a Data Package and add the "iris" data frame as a resource
my_package <-
  create_package() %>%
  add_resource(resource_name = "iris", data = iris)

my_package
#> A Data Package with 1 resource:
#> • iris
#> Use `unclass()` to print the Data Package as a list.

# Write the Data Package to disk
my_package %>%
  write_package("my_directory")
```

For more functionality, see [get
started](https://docs.ropensci.org/frictionless/articles/frictionless.html)
or the [function
reference](https://docs.ropensci.org/frictionless/reference/index.html).

## frictionless vs datapackage.r

[datapackage.r](https://github.com/frictionlessdata/datapackage-r) is an
alternative R package to work with Data Packages. It has an
object-oriented design and offers validation.

frictionless on the other hand allows you to quickly read and write Data
Packages to and from data frames, getting out of the way for the rest of
your analysis. It is designed to be lightweight, follows
[tidyverse](https://www.tidyverse.org/) principles and supports piping.
Its validation functionality is limited to what is needed for reading
and writing, see
[frictionless-py](https://github.com/frictionlessdata/frictionless-py)
for extensive validation.

## Meta

- We welcome
  [contributions](https://docs.ropensci.org/frictionless/CONTRIBUTING.html)
  including bug reports.
- License: MIT
- Get [citation
  information](https://docs.ropensci.org/frictionless/authors.html#citation)
  for frictionless in R doing `citation("frictionless")`.
- Please note that this project is released with a [Contributor Code of
  Conduct](https://frictionlessdata.io/work-with-us/code-of-conduct/).
  By participating in this project you agree to abide by its terms.
