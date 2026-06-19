#' Get Data Package version
#'
#' Determines what version of the [Data Package standard](
#' https://datapackage.org/) is used by a Data Package, based on the
#' [`$schema`](https://datapackage.org/standard/data-package/#dollar-schema)
#' property.
#' Version `"1.0"` is assumed if `$schema` is missing, version `">=2.0"` is
#' assumed for custom values (e.g. [extensions](
#' https://datapackage.org/standard/extensions/)).
#'
#' @inheritParams read_resource
#' @return Data Package version number (e.g. `1.0`).
#' @family version functions
#' @export
#' @examples
#' package <- example_package()
#' version(package)
version <- function(package) {
  dollar_schema <- purrr::pluck(package, "$schema")

  if (is.null(dollar_schema)) {
    return("1.0") # Assume 1.0 if $schema is undefined
  }
  if (!is.character(dollar_schema)) {
    return(">=2.0") # Assume 2.0 or higher if $schema is present but invalid
  }

  # Extract version from e.g.
  # "https://datapackage.org/profiles/<version>/datapackage.json"
  pattern = "^https://datapackage\\.org/profiles/((?:[0-9A-Za-z]|\\.|-)+)/.*"
  extracted_version <- sub(
    pattern,
    "\\1",
    dollar_schema,
    perl = TRUE
  )

  if (extracted_version == dollar_schema) {
    return(">=2.0") # Assume 2.0 or higher if $schema is a custom value
  }

  extracted_version
}
