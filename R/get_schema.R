#' Get the Table Schema of a Data Resource
#'
#' Returns the [Table Schema](https://specs.frictionlessdata.io/table-schema/)
#' of a Data Resource (in a Data Package), i.e. the content of its `schema`
#' property, describing the resource's fields, data types, relationships, and
#' missing values. The resource must be a
#' [Tabular Data Resource](https://specs.frictionlessdata.io/tabular-data-resource/).
#'
#' @inheritParams read_resource
#' @return List object describing a Table Schema.
#' @export
#' @examples
#' # Load the example Data Package
#' package <- example_package
#'
#' # Get the Table Schema for the resource "observations"
#' schema <- get_schema(package, "observations")
#' str(schema)
get_schema <- function(package, resource_name) {
  # Get resource
  resource <- get_resource(package, resource_name)

  # Check resource is tabular-data-resource (expected for resources with schema)
  assertthat::assert_that(
    replace_null(resource$profile, "") == "tabular-data-resource",
    msg = glue::glue(
      "Resource `{resource_name}` must have property `profile` with value",
      "`tabular-data-resource`.", .sep = " "
    )
  )

  # Get schema
  schema <- read_json(resource$schema, package$directory)

  # Check schema has fields
  fields <- schema$fields
  assertthat::assert_that(
    !is.null(fields),
    msg = glue::glue(
      "Resource `{resource_name}` must have property `schema` containing",
      "`fields`.", .sep = " "
    )
  )

  schema
}
