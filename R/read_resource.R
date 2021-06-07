#' Read data from a Data Package resource
#'
#' Reads data from a Data Package resource into a **tibble** (a Tidyverse data
#' frame). The resource has to meet the requirements of a [Tabular Data
#' Resource](https://specs.frictionlessdata.io/tabular-data-resource/). The
#' function is a wrapper around `readr::read_delim()`, passing the resource
#' properties `path`, CSV dialect, column names, data types, etc. Column names
#' are taken from the provided `schema`, not from the header in the CSV file(s).
#'
#' @param package Package object, see `read_package()`.
#' @param resource_name Name of the resource to load data from.
#'
#' @return Tibble with the resource data.
#'
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows recode tibble %>%
#' @importFrom glue glue
#' @importFrom httr http_error
#' @importFrom jsonlite fromJSON
#' @importFrom purrr keep map_chr map_dfr
#' @importFrom readr locale read_delim
#'
#' @details
#' The `resource` properties are handled as follows:
#'
#' ## Path
#'
#' [`path`](https://specs.frictionlessdata.io/data-resource/#data-location) is
#' required. It can be a local path or URL, which must resolve. When multiple
#' paths are provided (`"path": [ "myfile1.csv", "myfile2.csv"]`) then data are
#' merged into a single data frame, in the order in which the paths are listed.
#'
#' ## Data
#'
#' Inline `data` is not supported.
#'
#' ## Name
#'
#' [`name`](https://specs.frictionlessdata.io/data-resource/#name) is required.
#' It is used to find the resource with `name` = `resource_name`.
#'
#' ## Profile
#'
#' [`profile`](https://specs.frictionlessdata.io/tabular-data-resource/#specification)
#' is required to have the value `tabular-data-resource`.
#'
#' ## File encoding
#'
#' [`encoding`](https://specs.frictionlessdata.io/tabular-data-resource/#csv-file-requirements)
#' is required if resource files are not encoded as UTF-8. For proper values,
#' see
#' [encoding](https://specs.frictionlessdata.io/data-resource/#optional-properties).
#' The returned data frame will always be UTF-8.
#'
#' ## CSV Dialect
#'
#' `dialect` is required if the resource CSV file properties differ from the
#' defaults described in the [CSV Dialect
#' specification](https://specs.frictionlessdata.io/csv-dialect/#specification)
#' (i.e. comma separated, `"` to quote, etc.). The following CSV dialect
#' properties are not interpreted:
#' - `escapeChar`: if different than `\`.
#' - `lineTerminator`
#' - `nullSequence`
#' - `caseSensitiveHeader`
#' - `csvddfVersion`
#'
#' ## Schema
#'
#' `schema` is required and must follow the [Table
#' Schema](http://specs.frictionlessdata.io/table-schema/) specification.
#'
#' - Field `name`s are used as column headers.
#' - Field `type`s are used as column types when provided. Types are guessed
#'   when no type is provided or it has no equivalent in R.
#' - Field `format`s (especially for `date`, `time`, `datetime`) are ignored.
#' - [`missingValues`](https://specs.frictionlessdata.io/table-schema/#missing-values)
#'   are used to interpret as `NA`, with `""` as default.
#'
#' ## Ignored resource properties
#'
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
#' path <- system.file("extdata", "datapackage.json", package = "datapackage")
#' package <- read_package(path)
#' package$resource_names
#' read_resource(package, "observations")
read_resource <- function(package, resource_name) {
  # Select resource
  resource_names_collapse <- paste(package$resource_names, collapse = ", ")
  assert_that(
    resource_name %in% map_chr(package$resources, "name"),
    msg = glue(
      "Can't find resource `{resource_name}` in `{resource_names_collapse}`."
    )
  )
  resource <- keep(package$resources, function(x) {
    (x[["name"]] == resource_name)
  })[[1]]

  # Check if resource is `tabular-data-resource`
  assert_that(
    resource$profile == "tabular-data-resource",
    msg = glue(
      "Resource `{resource_name}` must have property `profile` with value ",
      "`tabular-data-resource`."
    )
  )

  # Select and verify path(s) to file(s)
  # https://specs.frictionlessdata.io/data-resource/#data-location
  assert_that(
    !is.null(resource$path),
    msg = glue(
      "Resource `{resource_name}` must have property `path`."
    )
  )
  paths <-
    resource$path %>%
    # If not URL, append directory to create a full path
    map_chr(function(path) {
      if (startsWith(path, "http")) {
        path
      } else {
        paste(package$directory, path, sep = "/")
      }
    })
  for (path in paths) {
    if (startsWith(path, "http")) {
      assert_that(
        !http_error(path),
        msg = glue("Can't find file at `{path}`.")
      )
    } else {
      assert_that(
        file.exists(path),
        msg = glue("Can't find file at `{path}`.")
      )
    }
  }

  # Select CSV dialect, see https://specs.frictionlessdata.io/csv-dialect/
  dialect <- resource$dialect # Can be NULL
  # Helper function to assign value when property is NULL
  if_null <- function(variable, value) {
    ifelse(!is.null(variable), variable, value)
  }

  # Select schema fields
  assert_that(
    !is.null(resource$schema$fields),
    msg = glue(
      "Resource `{resource_name}` must have property `schema` containing ",
      "`fields`."
    )
  )
  fields <- map_dfr(resource$schema$fields, function(x) {
    if ("name" %in% names(x)) {
      (name_value <- x[["name"]])
    } else {
      name_value <- NA_character_
    }
    if ("type" %in% names(x)) {
      (type_value <- x[["type"]])
    } else {
      type_value <- NA_character_
    }
    tibble(name = name_value, type = type_value)
  })
  assert_that(all(!is.na(fields$name)),
    msg = glue(
      "Field `{which(is.na(fields$name))}` of resource `{resource_name}` must ",
      "have the property `name`."
    )
  )
  field_names <- fields$name
  field_types <- fields$type

  # Recode field types
  field_types <- recode(field_types,
    "string" = "c", # Format (email, url) ignored
    "number" = "n", # TODO: extra properties
    "integer" = "i", # TODO: extra properties
    "boolean" = "l", # TODO: extra properties
    "object" = "?",
    "array" = "?",
    "date" = "D", # TODO: formats
    "time" = "t", # TODO: formats
    "datetime" = "T", # TODO: formats
    "year" = "f",
    "yearmonth" = "f",
    "duration" = "?",
    "geopoint" = "?",
    "geojson" = "?",
    "any" = "?",
    .default = "?", # Unrecognized type
    .missing = "?" # No type provided
  )

  # TODO: test header matching

  # Read data
  dataframes <- list()
  for (i in 1:length(paths)) {
    data <- read_delim(
      file = paths[i],
      delim = if_null(dialect$delimiter, ","),
      quote = if_null(dialect$quoteChar, "\""),
      escape_backslash = ifelse(
        if_null(dialect$escapeChar, "not set") == "\\", TRUE, FALSE
      ),
      escape_double = if_null(dialect$doubleQuote, TRUE),
      col_names = field_names,
      col_types = paste(field_types, collapse = ""),
      locale = locale(encoding = if_null(resource$encoding, "UTF-8")),
      na = if_null(resource$schema$missingValues, ""),
      quoted_na = TRUE,
      comment = if_null(dialect$commentChar, ""),
      trim_ws = if_null(dialect$skipInitialSpace, FALSE),
      # Skip header row when present
      skip = ifelse(if_null(dialect$header, TRUE), 1, 0),
      skip_empty_rows = TRUE
    )
    dataframes[[i]] <- data
  }

  bind_rows(dataframes)
}
