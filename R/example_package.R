#' Read the example Data Package
#'
#' Reads the example Data Package included in `frictionless`.
#' This dataset is used in examples, vignettes, and tests and contains dummy
#' camera trap data organized in 3 Data Resources:
#' 1. `deployments`: one local data file referenced in
#'    `"path": "deployments.csv"`.
#' 2. `observations`: two local data files referenced in
#'    `"path": ["observations_1.tsv", "observations_2.tsv"]`.
#' 3. `media`: inline data stored in `data`.
#'
#' The example Data Package is available in two versions:
#' - `1.0`: specified as a [Data Package v1](
#'   https://specs.frictionlessdata.io/).
#' - `2.0`: specified as a [Data Package v2](https://datapackage.org/).
#'
#' @param version Data Package standard version.
#' @return A Data Package object, see [create_package()].
#' @family sample data
#' @export
#' @examples
#' # Version 1
#' example_package()
#'
#' # Version 2
#' example_package(version = "2.0")
example_package <- function(version = "1.0") {
  version_dir <- switch(
    version %||% "1.0",
    "2.0" = "v2",
    "1.0" = "v1",
    "v1" # Use v1 for any other value
  )
  path <- system.file(
    "extdata", version_dir, "datapackage.json", package = "frictionless"
  )
  read_package(path)
}
