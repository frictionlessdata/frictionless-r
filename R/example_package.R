#' Read the example Data Package
#'
#' Reads the example Data Package included in `frictionless`.
#' This dataset is used in examples, vignettes, and tests and contains dummy
#' camera trap data organized in 3 Data Resources:
#' 1. `deployments`: one local data file referenced in
#'    `"path": "deployments.csv"`.
#' 2. `observations`: two local data files referenced in
#'    `"path": ["observations_1.csv", "observations_2.csv"]`.
#' 3. `media`: inline data stored in `data`.
#'
#' @return A Data Package object, see [create_package()].
#' @family sample data
#' @export
#' @examples
#' example_package()
example_package <- function() {
  path <- system.file("extdata", "datapackage.json", package = "frictionless")
  read_package(path)
}
