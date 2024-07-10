#' Read encoding
#'
#' Returns the encoding of a [Data
#' Resource](https://specs.frictionlessdata.io/data-resource/) (in a Data
#' Package), or sets it to UTF-8 if unknown.
#' A warning is returned if the given encoding is unknown.
#'
#' @inheritParams read_resource
#' @return Encoding of resource, or UTF-8 if unknown.
#' @family helper functions
#' @noRd
#' @examples
#' # Load the example Data Package
#' package <- example_package
#'
#' # Get encoding of resource "observations"
#' frictionless:::get_encoding(package, resource_name = "observations")
get_encoding <- function(package, resource_name) {
  encoding <- resource$encoding %||% "UTF-8" # Set default to UTF-8
  if (!tolower(encoding) %in% tolower(iconvlist())) {
    cli::cli_warn(
      "Unknown encoding {.field {encoding}}. Reading file(s) with UTF-8
         encoding.",
      class = "frictionless_warning_resource_encoding_unknown"
    )
    encoding <- "UTF-8"
  }
  return(encoding)
}
