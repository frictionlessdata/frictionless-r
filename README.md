
<!-- README.md is generated from README.Rmd. Please edit that file -->

# datapackage

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/datapackage)](https://CRAN.R-project.org/package=datapackage)
[![R-CMD-check](https://github.com/inbo/datapackage/workflows/R-CMD-check/badge.svg)](https://github.com/inbo/datapackage/actions)
[![codecov](https://codecov.io/gh/inbo/datapackage/branch/main/graph/badge.svg?token=bKtiHW21K0)](https://codecov.io/gh/inbo/datapackage)
<!-- badges: end -->

Datapackage is an R package to read and write [Frictionless Data
Packages](https://specs.frictionlessdata.io/data-package/) in a
[Tidyverse](https://www.tidyverse.org/) way.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("inbo/datapackage")
```

## Usage

There are two functions: `read_package()` and `read_resource()`.

### Basic functionality

``` r
library(datapackage)

# Read datapackage.json file
package <- read_package(system.file("extdata", "datapackage.json", package = "datapackage"))

# List resource names
package$resource_names
#> [1] "deployments"  "observations"

# Read data from resource "observations"
read_resource(package, "observations")
#> # A tibble: 8 x 7
#>   observation_id   deployment_id timestamp           scientific_name count age  
#>   <chr>            <chr>         <dttm>              <chr>           <dbl> <chr>
#> 1 089113c5-e279-4… 1             2020-01-06 15:59:17 Capreolus capr…     1 juve…
#> 2 060ab157-3a8e-4… 1             2020-01-06 15:59:17 Capreolus capr…     1 adult
#> 3 aba72fdc-2fd8-4… 1             2020-01-06 16:35:23 Lepus europaeus     1 adult
#> 4 1c044f0c-b12b-4… 1             2020-01-06 17:04:04 Lepus europaeus     1 adult
#> 5 a2fb27ab-bb8b-4… 1             2020-01-06 19:19:54 Sus scrofa          2 unde…
#> 6 cdbb4a57-2204-4… 2             2021-01-01 01:25:06 Sus scrofa          1 unde…
#> 7 3806c18c-29c5-4… 2             2021-01-01 01:25:06 Sus scrofa          1 unde…
#> 8 aa01aa7c-10dc-4… 2             2021-01-01 04:47:30 Sus scrofa          1 unde…
#> # … with 1 more variable: comments <chr>
```

### Read external data

Datapackage allows you to access all data from an external Frictionless
Data Package (e.g. one published on
[Zenodo](https://zenodo.org/search?page=1&size=20&q=frictionlessdata&type=dataset))
via the `datapackage.json` URL:

``` r
# Read datapackage.json file: this will give you access to all data resources
# without reading them, which is convenient and fast
package <- read_package("https://zenodo.org/record/5070086/files/datapackage.json")
#> Please make sure you have the right to access data from this Data Package for your proposed use. Follow applicable norms or requirements to credit the dataset and its authors.
#> For more information, see https://doi.org/10.5281/zenodo.5070086

# List resource names
package$resource_names
#> [1] "reference-data" "gps"            "acceleration"

# Read gps data: will return a single data frame, even though data are 
# split over multiple csv files
read_resource(package, "gps")
#> # A tibble: 73,047 x 21
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
#> #   tag-local-identifier <chr>, individual-local-identifier <chr>,
#> #   study-name <chr>
```
