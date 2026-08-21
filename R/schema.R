#' Get the Table Schema of a Data Resource
#'
#' Gets the Table Schema of a Data Resource (in a Data Package), i.e. the
#' content of its `schema` property, describing the resource's fields, data
#' types, relationships, and missing values.
#' The resource must be a [Tabular Data Resource](
#' https://specs.frictionlessdata.io/tabular-data-resource/).
#'
#' See `vignette("table-schema")` to learn more about Table Schema.
#'
#' @inheritParams read_resource
#' @returns List describing a Table Schema.
#' @family accessor functions
#' @export
#' @examples
#' # Load the example Data Package
#' package <- example_package()
#'
#' # Get the Table Schema for the resource "observations"
#' schema <- schema(package, "observations")
#' str(schema)
schema <- function(package, resource_name) {
  # Get resource
  resource <- resource(package, resource_name, upgrade = TRUE)

  # Check resource is tabular (expected for resources with schema)
  if (resource$type %||% "" != "table") {
    cli::cli_abort(
      "Resource {.val {resource_name}} must have a {.field type} property
       with value {.val table}.",
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
  schema <- read_descriptor(
    resource$schema,
    attr(package, "directory"),
    safe = TRUE
  )

  # Check schema
  check_schema(schema)

  return(schema)
}
