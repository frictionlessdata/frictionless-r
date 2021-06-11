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
#' is required if resource files are not encoded as UTF-8. For proper values
#' (e.g. `windows-1252`), see "Preferred MIME Names" in
#' [encoding](https://specs.frictionlessdata.io/data-resource/#optional-properties).
#' The returned data frame will always be UTF-8.
#'
#' ## CSV Dialect
#'
#' `dialect` is required if the resource CSV file properties differ from the
#' defaults described in the [CSV Dialect
#' specification](https://specs.frictionlessdata.io/csv-dialect/#specification)
#' (i.e. comma separated, `"` to quote, etc.).
#'
#' For `escapeChar`, only `"escapeChar": "\\"` is supported and it will ignore
#' `"doubleChar": "true"` as these fields are mutually exclusive.
#'
#' The following CSV dialect properties are not interpreted: `lineTerminator`,
#' `nullSequence`, `caseSensitiveHeader`, and `csvddfVersion`.
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
  # Helper function to assign value when property is NULL
  replace_null <- function(value, replace) {
    if(!is.null(value)) { value } else { replace }
  }

  # Check package
  assert_that(
    class(package) == "list",
    msg = glue(
      "`package` must be a list object containing descriptor information, ",
      "see read_package()."
    )
  )
  assert_that(
    !is.null(package$resource_names),
    msg = glue(
      "`package` must have property `resource_names`, load with read_package()."
    )
  )

  # Select resource
  resource_names_collapse <- paste(package$resource_names, collapse = ", ")
  assert_that(
    resource_name %in% package$resource_names,
    msg = glue(
      "Can't find resource `{resource_name}` in `{resource_names_collapse}`."
    )
  )
  resource <- keep(package$resources, function(x) {
    (x[["name"]] == resource_name)
  })[[1]]

  # Check if resource is `tabular-data-resource`
  assert_that(
    replace_null(resource$profile, "") == "tabular-data-resource",
    msg = glue(
      "Resource `{resource_name}` must have property `profile` with value ",
      "`tabular-data-resource`."
    )
  )

  # Select and verify path(s) to file(s)
  # https://specs.frictionlessdata.io/data-resource/#data-location
  assert_that(
    !is.null(resource$path),
    msg = glue("Resource `{resource_name}` must have property `path`.")
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
      "Field {which(is.na(fields$name))} of resource `{resource_name}` must ",
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

  # Select CSV dialect, see https://specs.frictionlessdata.io/csv-dialect/
  dialect <- resource$dialect # Can be NULL

  # Read data
  dataframes <- list()
  for (i in 1:length(paths)) {
    data <- read_delim(
      file = paths[i],
      delim = replace_null(dialect$delimiter, ","),
      quote = replace_null(dialect$quoteChar, "\""),
      escape_backslash = ifelse(
        replace_null(dialect$escapeChar, "not set") == "\\", TRUE, FALSE
      ),
      escape_double = ifelse(
        # if escapeChar is set, set doubleQuote to FALSE (mutally exclusive)
        replace_null(dialect$escapeChar, "not set") == "\\",
        FALSE,
        replace_null(dialect$doubleQuote, TRUE)
      ),
      col_names = replace_null(dialect$header, TRUE), # Fields names applied later
      col_types = paste(field_types, collapse = ""),
      locale = locale(encoding = replace_null(resource$encoding, "UTF-8")),
      na = replace_null(resource$schema$missingValues, ""),
      quoted_na = TRUE,
      comment = replace_null(dialect$commentChar, ""),
      trim_ws = replace_null(dialect$skipInitialSpace, FALSE),
      skip = 0,
      skip_empty_rows = TRUE
    )

    # Warn for header <> schema mismatch when header is present (default)
    if (replace_null(dialect$header, TRUE) &
        !identical(colnames(data), field_names)) {
      field_names_collapse <- paste(field_names, collapse = ", ")
      colnames_collapse <- paste(colnames(data), collapse = ", ")
      warning(
        glue(
          "Mismatch between `schema$fields` and headers in file `{paths[i]}`:\n",
          "schema (used):\n",
          "  {field_names_collapse}\n",
          "headers (ignored):\n",
          "  {colnames_collapse}"
        )
      )
    }

    # Use schema field names in output
    colnames(data) <- field_names
    dataframes[[i]] <- data
  }

  bind_rows(dataframes)
}
