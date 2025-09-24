#' Get the Table Schema of a Data Resource
#'
#' Returns the Table Schema of a Data Resource (in a Data Package), i.e. the
#' content of its `schema` property, describing the resource's fields, data
#' types, relationships, and missing values.
#' The resource must be a [Tabular Data Resource](
#' https://specs.frictionlessdata.io/tabular-data-resource/).
#'
#' See `vignette("table-schema")` to learn more about Table Schema.
#'
#' @inheritParams read_resource
#' @return List describing a Table Schema.
#' @family accessor functions
#' @export
#' @examples
#' # Load the example Data Package
#' package <- example_package()
#'
#' # Get the Table Schema for the resource "observations"
#' schema <- get_schema(package, "observations")
#' str(schema)
get_schema <- function(package, resource_name) {
  # Get resource
  resource <- resource(package, resource_name)

  # Check resource is tabular-data-resource (expected for resources with schema)
  if (resource$profile %||% "" != "tabular-data-resource") {
    cli::cli_abort(
      "Resource {.val {resource_name}} must have a {.field profile} property
       with value {.val tabular-data-resource}.",
      class = "frictionless_error_resource_not_tabular"
    )
  }

  # Get schema
  if (is.null(resource$schema)) {
    cli::cli_abort(
      "Resource {.val {resource_name}} must have a {.field schema} property.",
      class = "frictionless_error_resource_without_schema"
    )
  }
  schema <- read_descriptor(resource$schema, package$directory, safe = TRUE)

  # Check schema
  check_schema(schema)

  return(schema)
}
