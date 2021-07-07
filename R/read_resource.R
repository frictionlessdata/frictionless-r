#' Read data from a Tabular Data Resource into a tibble
#'
#' Reads data from a Data Package **resource** into a tibble (a Tidyverse data
#' frame). The resource has to meet the requirements of a [Tabular Data
#' Resource](https://specs.frictionlessdata.io/tabular-data-resource/). The
#' function is a wrapper around [readr::read_delim()], passing the resource
#' properties `path`, CSV dialect, column names, data types, etc. Column names
#' are taken from the provided `schema`, not from the header in the CSV file(s).
#'
#' @param package Package object, see `read_package()`.
#' @param resource_name Name of the resource to load data from.
#'
#' @return A [tibble()] with the resource data.
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
#' The [`resource`](https://specs.frictionlessdata.io/data-resource/) properties
#' are handled as follows:
#'
#' ## Path
#'
#' [`path`](https://specs.frictionlessdata.io/data-resource/#data-location) is
#' required. It can be a local path or URL, which must resolve. Absolute path
#' (`/`) and relative parent path (`../`) are forbidden to avoid security
#' vulnerabilities.
#'
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
#' `name` is [required](https://specs.frictionlessdata.io/data-resource/#name).
#' It is used to find the resource with `name` = `resource_name`.
#'
#' ## Profile
#'
#' `profile` is
#' [required](https://specs.frictionlessdata.io/tabular-data-resource/#specification)
#' to have the value `tabular-data-resource`.
#'
#' ## File encoding
#'
#' `encoding` is required if the resource file(s) are not encoded as UTF-8. For
#' proper values (e.g. `windows-1252`), see "Preferred MIME Names" in
#' [encoding](https://specs.frictionlessdata.io/data-resource/#optional-properties).
#' The returned data frame will always be UTF-8.
#'
#' ## CSV Dialect
#'
#' `dialect` properties are
#' [required](https://specs.frictionlessdata.io/csv-dialect/#specification) if
#' the resource file(s) deviate from the default CSV settings (see below). Only
#' deviating properties need to be specified, e.g. a tab delimited file without
#' a header row needs:
#' ```json
#' "dialect": {"delimiter": "\t", "header": "false"}
#' ```
#'
#' These are the CSV dialect properties. Some are ignored by the function:
#' - `delimiter`: default `,`
#' - `lineTerminator`: ignored, line endings `LF`, `CRLF` and `CR` are
#' interpreted automatically by [readr::read_delim()].
#' - `quoteChar`: default `"`
#' - `doubleQuote`: default `true`
#' - `escapeChar`: anything but `\` is ignored and it will set `doubleQuote` to
#' `false` as these fields are mutually exclusive. You can thus not escape with
#' `\"` and `""` in the same file.
#' - `nullSequence`: ignored, use `missingValues`.
#' - `skipInitialSpace`: default `false`
#' - `header`: default `true`
#' - `commentChar`: not set by default.
#' - `caseSensitiveHeader`: ignored, header is not used for column names, see
#' Schema.
#' - `csvddfVersion`: ignored.
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
#' ## File compression
#'
#' Resource file(s) with `path` ending in `.gz`, `.bz2`, `.xz`, or `.zip` are
#' automatically decompressed using default [readr::read_delim()]
#' functionality. Only `.gz` files can be read directly from URL `path`s.
#' Only the extension in `path` can be used to indicate compression type,
#' the `compression` property is
#' [ignored](https://specs.frictionlessdata.io/patterns/#specification-3).
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
#' # Read datapackage.json file
#' package <- read_package(system.file("extdata", "datapackage.json", package = "datapackage"))
#'
#' # List resource names
#' package$resource_names
#'
#' # Read data from resource "observations"
#' read_resource(package, "observations")
#'
#' # The above tibble is merged from 2 files listed in the resource path
#' package$resources[[2]]$path
#'
#' # With col_names and col_types derived from the resource schema
#' purrr::map_chr(package$resources[[2]]$schema$fields, "name")
#' purrr::map_chr(package$resources[[2]]$schema$fields, "type")
read_resource <- function(package, resource_name) {
  # Helper function to assign value when property is NULL
  replace_null <- function(value, replace) {
    if(!is.null(value)) { value } else { replace }
  }

  # Check package
  assert_that(
    class(package) == "list",
    msg = glue(
      "`package` must be a list object containing descriptor information,",
      "see read_package().", .sep = " "
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
      "Resource `{resource_name}` must have property `profile` with value",
      "`tabular-data-resource`.", .sep = " "
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
        assert_that(
          !startsWith(path, "/"),
          msg = glue(
            "{path} is an absolute path (`/`) which is forbidden to avoid",
            "security vulnerabilities.", .sep = " "
          )
        )
        assert_that(
          !startsWith(path, "../"),
          msg = glue(
            "{path} is a relative parent path (`../`) which is forbidden to",
            "avoid security vulnerabilities.", .sep = " "
          )
        )
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
  fields <- resource$schema$fields
  assert_that(
    !is.null(fields),
    msg = glue(
      "Resource `{resource_name}` must have property `schema` containing",
      "`fields`.", .sep = " "
    )
  )

  # Get col_names and col_types
  col_names <- map_chr(fields, function(x) {
    replace_null(x[["name"]], NA_character_)
  })
  col_types <- map_chr(fields, function(x) {
    replace_null(x[["type"]], NA_character_)
  })
  assert_that(all(!is.na(col_names)),
    msg = glue(
      "Field {which(is.na(col_names))} of resource `{resource_name}` must",
      "have the property `name`.", .sep = " "
    )
  )

  # Recode col_types
  col_types <- recode(col_types,
    "string" = "c", # Format (email, url) ignored
    "number" = "n",
    "integer" = "n", # Not integer to avoid .Machine$integer.max overflow issues
    "boolean" = "l",
    "object" = "?",
    "array" = "?",
    "date" = "D",
    "time" = "t",
    "datetime" = "T",
    "year" = "f",
    "yearmonth" = "f",
    "duration" = "?",
    "geopoint" = "?",
    "geojson" = "?",
    "any" = "?",
    .default = "?", # Unrecognized type
    .missing = "?" # No type provided
  )

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
        # if escapeChar is set, set doubleQuote to FALSE (mutually exclusive)
        replace_null(dialect$escapeChar, "not set") == "\\",
        FALSE,
        replace_null(dialect$doubleQuote, TRUE)
      ),
      col_names = col_names,
      col_types = paste(col_types, collapse = ""),
      locale = locale(encoding = replace_null(resource$encoding, "UTF-8")),
      na = replace_null(resource$schema$missingValues, ""),
      quoted_na = TRUE,
      comment = replace_null(dialect$commentChar, ""),
      trim_ws = replace_null(dialect$skipInitialSpace, FALSE),
      # Skip header row when present
      skip = ifelse(replace_null(dialect$header, TRUE), 1, 0),
      skip_empty_rows = TRUE
    )
    dataframes[[i]] <- data
  }

  bind_rows(dataframes)
}
