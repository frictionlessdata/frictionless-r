
<!-- README.md is generated from README.Rmd. Please edit that file -->

# datapackage

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/datapackage)](https://CRAN.R-project.org/package=datapackage)
[![R-CMD-check](https://github.com/inbo/datapackage/workflows/R-CMD-check/badge.svg)](https://github.com/inbo/datapackage/actions)
<!-- badges: end -->

datapackage is an R package to read and write [Frictionless Data
Packages](https://frictionlessdata.io/data-package/) in a
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

``` r
library(datapackage)

# Read datapackage.json file
package <- read_package(system.file("extdata", "datapackage.json", package = "datapackage"))

# List resource names
package$resource_names
#> [1] "deployments"  "observations"

# Read data from resource
read_resource(package, "observations")
#> # A tibble: 8 x 7
#>   observation_id   deployment_id timestamp           scientific_name count age  
#>   <chr>            <chr>         <dttm>              <chr>           <int> <chr>
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
