#' Get the specification version number
#'
#' @description
#' Determines what version of the Data Package, Data Resource, Table Dialect or
#' Table Schema standard is used, based on the [`$schema`](
#' https://datapackage.org/standard/data-package/#dollar-schema) property.
#' - `"1.0"`: [v1](https://specs.frictionlessdata.io/) specification.
#'   Assumed if `$schema` is missing.
#' - `"2.0`: [v2](https://datapackage.org/) specification.
#' - `">=2.0"`: assumed if `$schema` is defined by deviating from the default
#'   (e.g. an [extension](https://datapackage.org/standard/extensions/)).
#'
#' @param x A list describing either a Data Package, Data Resource, Table
#'   Dialect or Table Schema.
#' @returns Data Package standard version number.
#' @family version functions
#' @export
#' @examples
#' # Data Package
#' package <- example_package()
#' version(package)
#'
#' # Data Resource
#' resource <- resource(package, "observations")
#' version(resource)
#'
#' # Table Dialect
#' version(resource$dialect)
#'
#' # Table Schema
#' schema <- schema(package, "observations")
#' version(schema)
version <- function(x) {
  dollar_schema <- purrr::pluck(x, "$schema")

  if (is.null(dollar_schema)) {
    return("1.0") # Assume 1.0 if $schema is undefined
  }
  if (!is.character(dollar_schema)) {
    return(">=2.0") # Assume 2.0 or higher if $schema is present but invalid
  }

  # Extract version from e.g.
  # "https://datapackage.org/profiles/1.0/datapackage.json" or
  # "https://datapackage.org/profiles/2.0/tableschema.json"
  pattern <- "^https://datapackage\\.org/profiles/((?:[0-9A-Za-z]|\\.|-)+)/.*"
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
