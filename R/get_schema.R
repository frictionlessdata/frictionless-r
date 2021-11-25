#' Get the Table Schema of a Data Resource
#'
#' Returns the [Table Schema](https://specs.frictionlessdata.io/table-schema/)
#' of a Data Resource (in a Data Package), i.e. the content of its `schema`
#' property, describing the resource's fields, data types, relationships, and
#' missing values. The resource must be a
#' [Tabular Data Resource](https://specs.frictionlessdata.io/tabular-data-resource/).
#'
#' @param resource_name Name of the resource.
#' @param package Data Package object, see `read_package()`.
#'
#' @return List object describing the Table Schema.
#'
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom glue glue
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' # Read datapackage.json file
#' package <- read_package(system.file("extdata", "datapackage.json", package = "frictionless"))
#'
#' # Get table schema for resource "observations"
#' schema <- get_schema("observations", package)
#' str(schema)
get_schema <- function(resource_name, package) {
  # Get resource
  resource <- get_resource(resource_name, package)

  # Check resource is tabular-data-resource (expected for resources with schema)
  assert_that(
    replace_null(resource$profile, "") == "tabular-data-resource",
    msg = glue(
      "Resource `{resource_name}` must have property `profile` with value",
      "`tabular-data-resource`.", .sep = " "
    )
  )

  # Get schema
  schema <- resource$schema
  if (is.character(schema)) {
    schema <- check_path(schema, directory = package$directory, unsafe = FALSE)
    schema <- fromJSON(schema, simplifyDataFrame = FALSE)
  }

  # Check schema has fields
  fields <- schema$fields
  assert_that(
    !is.null(fields),
    msg = glue(
      "Resource `{resource_name}` must have property `schema` containing",
      "`fields`.", .sep = " "
    )
  )

  schema
}
