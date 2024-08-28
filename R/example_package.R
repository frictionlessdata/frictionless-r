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
#' @param version Version of the [Data Package](
#' https://datapackage.org/blog/2024-06-26-v2-release/) standard that this
#' example Data Package uses.
#' @return A Data Package object, see [create_package()].
#' @family sample data
#' @export
#' @examples
#' example_package()
example_package <- function(version = "1.0") {
  supported_versions <- c("1.0", "2.0")
  if (!version  %in% supported_versions) {
    cli::cli_abort(
      c(
        "{.val {version}} is not a supported Data Package version.",
        "i" = "Supported version{?s}: {.val {supported_versions}}."
      ),
      class = "frictionless_error_unsupported_version"
    )
  }
  if (version == "1.0") {
    path <- system.file(
      "extdata", "v1", "datapackage.json", package = "frictionless"
    )
  } else {
    path <- system.file(
      "extdata", "v2", "datapackage.json", package = "frictionless"
    )
  }
  read_package(path)
}
