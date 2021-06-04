#' Read data from a Data Package resource
#'
#' Loads data from a resource of a Data Package into a **tibble** (a Tidyverse
#' data frame). The resource has to meet the requirements of a
#' [Tabular Data Resource](https://specs.frictionlessdata.io/tabular-data-resource/).
#' The function is a wrapper around `readr::read_delim()`, passing the resource
#' properties (`path`, CSV dialect, field names, data types, etc.) as good as
#' possible. Column names are taken from the provided `schema`, not from the
#' header in the CSV file(s).
#'
#' @param descriptor Descriptor object (see `read_descriptor()`).
#' @param resource_name Name of the resource to load data from.
#'
#' @return tibble with the resource data.
#'
#' @details
#' ## CSV dialect
#'
#' Resource files are required to follow the
#' [CSV file requirements](https://specs.frictionlessdata.io/tabular-data-resource/#csv-file-requirements)
#' unless a `dialect` is provided.
#' [CSV dialect properties](https://specs.frictionlessdata.io/csv-dialect/#specification)
#' are interpreted when provided, otherwise default values are used. Following
#' properties are ignored:
#' - `escapeChar` if different than `\`.
#' - `lineTerminator`: TODO
#' - `nullSequence`
#' - `caseSensitiveHeader`: Table Schema field names are used instead
#' - `csvddfVersion`
#'
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr recode if_else
#' @importFrom jsonlite fromJSON
#' @importFrom readr read_delim
#'
#' @examples
#' \dontrun{
#' descriptor <- read_descriptor(system.file("extdata", "datapackage.json", package = "datapackage"))
#' df <- read_resource(descriptor, "deployments")
#' }
read_resource <- function(descriptor, resource_name) {
  # Resource is listed in `resources`
  assert_that(resource_name %in% descriptor$resource_names,
    msg = paste0("Can't find resource \"", resource_name, "\"")
  )

  resource <- descriptor$resources[[1]] # TODO: select resource

  # Resource is `tabular-data-resource`
  assert_that(resource$profile == "tabular-data-resource",
    msg = paste0("Resource \"", resource_name, "\" is not defined as a tabular-data-resource.")
  )

  # Path(s) to file
  if (startsWith(resource$path, "http")) {
    path <- resource$path
  } else {
    path <- paste(descriptor$directory, resource$path, sep = "/")
  }
  # TODO: deal with multiple paths

  # CSV `dialect`, see https://specs.frictionlessdata.io/csv-dialect/
  dialect <- resource$dialect # Can be NULL
  # Helper function to assign value when property is NULL
  if_null <- function(variable, value) {
    if_else(!is.null(variable), variable, value)
  }

  # Field names
  field_names <- map_chr(resource$schema$fields, "name") # TODO: fail when name not provided
  field_types <- map_chr(resource$schema$fields, "type") # TODO: type not required
  field_types <- recode(field_types,
     "string" = "c", # Format (email, url) ignored
     "number" = "n", # TODO: extra properties
     "integer" = "i", # TODO: extra properties
     "boolean" = "l", # TODO: extra properties
     "object" = "c", # Different
     "array" = "c", # Different
     "date" = "D", # TODO: formats
     "time" = "t", # TODO: formats
     "datetime" = "T", # TODO: formats
     "year" = "f", # Different
     "yearmonth" = "f", # Different
     "duration" = "c", # Different
     "geopoint" = "c", # Different
     "geojson" = "c", # Different
     "any" = "c",
     .default = "c" # Unrecognized type
  )

  data <- read_delim(
    file = path,
    delim = if_null(dialect$delimiter, ","),
    quote = if_null(dialect$quoteChar, "\""),
    escape_backslash = if_else(if_null(dialect$escapeChar, "not set") == "\\", TRUE, FALSE),
    escape_double = if_null(dialect$doubleQuote, TRUE),
    col_names = field_names,
    col_types = paste(field_types, collapse = ""),
    # TODO: locale, including encoding
    # TODO: na <- read from table schema
    quoted_na = TRUE,
    comment = if_null(dialect$commentChar, ""),
    trim_ws = if_null(dialect$skipInitialSpace, FALSE),
    skip = if_else(if_null(dialect$header, TRUE), 1, 0), # Skip header row
    skip_empty_rows = TRUE
  )

  # TODO: Must have table schema
  # TODO: CSV file encoding
  # TODO: JSON row arrays
  # TODO: JSON row objects

  data
}
