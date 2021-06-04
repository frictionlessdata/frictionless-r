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
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr if_else recode %>%
#' @importFrom jsonlite fromJSON
#' @importFrom purrr keep map_chr
#' @importFrom RCurl url.exists
#' @importFrom readr locale read_delim
#'
#' @details
#' The `resource` properties are handled as follows:
#'
#' ## Path
#'
#' `path` is required. It can be a local path or URL, which must resolve.
#' When multiple paths are provided (`"path": [ "myfile1.csv", "myfile2.csv"]`)
#' then data are merged into a single data frame, in the order in which the
#' paths are listed.
#'
#' ## Data
#'
#' Inline `data` is not supported.
#'
#' ## Name
#'
#' `name` is required. It is used to find the resource with `name` =
#' `resource_name`.
#'
#' ## Profile
#'
#' `profile` is required to have the value `tabular-data-resource`.
#'
#' #' ## File encoding
#'
#' `encoding` is required if resource files are not encoded as UTF-8. For proper
#' values, see [encoding](https://specs.frictionlessdata.io/data-resource/#optional-properties).
#' The returned data frame will always be UTF-8.
#'
#' ## CSV Dialect
#'
#' If no `dialect` is provided, then resource CSV files are required to follow
#' the [CSV file requirements](https://specs.frictionlessdata.io/tabular-data-resource/#csv-file-requirements).
#'
#' If a `dialect` is provided, then the properties are interpreted as described
#' in the [CSV Dialect properties](https://specs.frictionlessdata.io/csv-dialect/#specification),
#' with the same defaults. Exceptions are:
#' - `escapeChar`: ignored if different than `\`.
#' - `lineTerminator`: ignored
#' - `nullSequence`: ignored
#' - `caseSensitiveHeader`: ignored,
#' - `csvddfVersion`: ignored
#'
#' ## Schema
#'
#' `schema` is required and must follow the
#' [Table Schema](http://specs.frictionlessdata.io/table-schema/) specification.
#'
#' - Field `name`s are used as column headers.
#' - Field `type`s are used as column types if provided. Unknown R types are cast
#' to `character`.
#'
#' ## Ignored resource properties
#'
#' The following properties are ignored:
#' - `title`
#' - `description`
#' - `format`
#' - `mediatype`
#' - `bytes`
#' - `hash`
#' - `sources`
#' - `licenses`
#'
#' @examples
#' descriptor <- read_descriptor(system.file("extdata", "datapackage.json", package = "datapackage"))
#' df <- read_resource(descriptor, "observation")
read_resource <- function(descriptor, resource_name) {
  # Select resource
  assert_that(
    resource_name %in% descriptor$resource_names, # TODO: rely on this property?
    msg = paste0("Can't find resource \"", resource_name, "\"")
  )
  resource <- keep(descriptor$resources, function(x) {
    (x[["name"]] == resource_name)
  })[[1]]

  # Check if resource is `tabular-data-resource`
  assert_that(
    resource$profile == "tabular-data-resource",
    msg = paste0("Resource \"", resource_name, "\" is not defined as a `tabular-data-resource`.")
  )

  # Select and verify path(s) to file(s)
  assert_that(
    !is.null(resource$path),
    msg = paste0("Resource \"", resource_name, "\" does not have a `path` property.")
  )
  paths <-
    resource$path %>%
    # If not URL, append directory to create a full path
    map_chr(function(path) if_else(
      startsWith(path, "http"), path, paste(descriptor$directory, path, sep = "/")
    ))
  for (path in paths) {
    assert_that(
      url.exists(path) | file.exists(path),
      msg = paste0("No file at ", path)
    )
  }

  # Select CSV dialect, see https://specs.frictionlessdata.io/csv-dialect/
  dialect <- resource$dialect # Can be NULL
  # Helper function to assign value when property is NULL
  if_null <- function(variable, value) {
    ifelse(!is.null(variable), variable, value)
  }

  # Select schema
  assert_that(
    !is.null(resource$schema),
    msg = paste0("Resource \"", resource_name, "\" does not have a `schema` property.")
  )

  # Select field names
  field_names <- map_chr(resource$schema$fields, "name") # TODO: fail when name not provided

  # Select field types
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
    escape_backslash = ifelse(if_null(dialect$escapeChar, "not set") == "\\", TRUE, FALSE),
    escape_double = if_null(dialect$doubleQuote, TRUE),
    col_names = field_names,
    col_types = paste(field_types, collapse = ""),
    locale = locale(encoding = if_null(resource$encoding, "UTF-8")),
    # TODO: na <- read from table schema
    quoted_na = TRUE,
    comment = if_null(dialect$commentChar, ""),
    trim_ws = if_null(dialect$skipInitialSpace, FALSE),
    skip = ifelse(if_null(dialect$header, TRUE), 1, 0), # Skip header row
    skip_empty_rows = TRUE
  )

  # TODO: Must have table schema
  # TODO: JSON row arrays
  # TODO: JSON row objects
  # TODO: use ifelse if_else consistently

  data
}
