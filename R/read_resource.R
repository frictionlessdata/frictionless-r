#' Read data from a Data Resource into a tibble data frame
#'
#' Reads data from a Data Resource (in a Data Package) into a tibble (a
#' Tidyverse data frame). The resource must be a
#' [Tabular Data Resource](https://specs.frictionlessdata.io/tabular-data-resource/).
#' The function is a wrapper around [readr::read_delim()], passing the resource
#' properties `path`, CSV dialect, column names, data types, etc. Column names
#' are taken from the provided `schema`, not from the header in the CSV file(s).
#'
#' @param resource_name Name of the resource.
#' @param package List object describing a Data Package, created with
#'   [read_package()] or [create_package()].
#' @return [dplyr::tibble()] data frame with the resource data.
#' @export
#' @section Resource properties:
#' The [resource properties](https://specs.frictionlessdata.io/data-resource/)
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
#' - `lineTerminator`: ignored, line terminator characters `LF` and `CRLF` are
#' interpreted automatically by [readr::read_delim()], while `CR` (used by
#' Classic Mac OS, final release 2001) is not supported.
#' - `doubleQuote`: default `true`
#' - `quoteChar`: default `"`
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
#' @section Table schema properties:
#' `schema` is required and must follow the [Table
#' Schema](http://specs.frictionlessdata.io/table-schema/) specification. It
#' can either be a JSON object or a URL or path referencing a JSON object.
#'
#' - Field `name`s are used as column headers.
#' - Field `type`s are use as column types (see further).
#' - [`missingValues`](https://specs.frictionlessdata.io/table-schema/#missing-values)
#' are used to interpret as `NA`, with `""` as default.
#'
#' ## Field types
#'
#' [strptime]: https://docs.python.org/2/library/datetime.html#strftime-strptime-behavior
#'
#' Field `type` is used to set the column type, as follows:
#'
#' - [string](https://specs.frictionlessdata.io/table-schema/#string) →
#' `character`; or `factor` when `enum` is present. `format` is ignored.
#' - [number](https://specs.frictionlessdata.io/table-schema/#number) →
#' `double`; or `factor` when `enum` is present. Use `bareNumber: false` to
#' ignore whitespace and non-numeric characters. `decimalChar` (`.` by default)
#' and `groupChar` (undefined by default) can be defined, but the most occurring
#' value will be used as a global value for all number fields of that resource.
#' - [integer](https://specs.frictionlessdata.io/table-schema/#integer) →
#' `double` (not integer, to avoid issues with big numbers); or `factor` when
#' `enum` is present. Use `bareNumber: false` to ignore whitespace and
#' non-numeric characters.
#' - [boolean](https://specs.frictionlessdata.io/table-schema/#boolean) →
#' `logical`. Non-default `trueValues/falseValues` are not supported.
#' - [object](https://specs.frictionlessdata.io/table-schema/#object) →
#' `character`.
#' - [array](https://specs.frictionlessdata.io/table-schema/#array) →
#' `character`.
#' - [date](https://specs.frictionlessdata.io/table-schema/#date) → `date`.
#' Supports `format`, with values `default` (ISO date), `any` (guess `ymd`) and
#' [Python/C strptime][strptime] patterns, such as `%a, %d %B %Y` for `Sat, 23
#' November 2013`. `%x` is `%m/%d/%y`. `%j`, `%U`, `%w` and `%W` are not
#' supported.
#' - [time](https://specs.frictionlessdata.io/table-schema/#time) →
#' [hms::hms()]. Supports `format`, with values `default` (ISO time), `any`
#' (guess `hms`) and [Python/C strptime][strptime] patterns, such as
#' `%I%p%M:%S.%f%z` for `8AM30:00.300+0200`.
#' - [datetime](https://specs.frictionlessdata.io/table-schema/#datetime) →
#' `POSIXct`. Supports `format`, with values `default` (ISO datetime), `any`
#' (ISO datetime) and the same patterns as for `date` and `time`. `%c` is not
#' supported.
#' - [year](https://specs.frictionlessdata.io/table-schema/#year) → `date`,
#' with `01` for month and day.
#' - [yearmonth](https://specs.frictionlessdata.io/table-schema/#yearmonth) →
#' `date`, with `01` for day.
#' - [duration](https://specs.frictionlessdata.io/table-schema/#duration) →
#' `character`. Can be parsed afterwards with [lubridate::duration()].
#' - [geopoint](https://specs.frictionlessdata.io/table-schema/#geopoint) →
#' `character`.
#' - [geojson](https://specs.frictionlessdata.io/table-schema/#geojson) →
#' `character`.
#' - [any](https://specs.frictionlessdata.io/table-schema/#any) → `character`.
#' - no type provided → type is guessed.
#' - unknown type → type is guessed.
#' @examples
#' # Read a datapackage.json file
#' package <- read_package(
#'   system.file("extdata", "datapackage.json", package = "frictionless")
#' )
#'
#' # List the resource names
#' package$resource_names
#'
#' # Read data from the resource "observations"
#' read_resource("observations", package)
#'
#' # The above tibble is merged from 2 files listed in the resource path
#' package$resources[[2]]$path
#'
#' # With col_names and col_types derived from the resource schema
#' purrr::map_chr(package$resources[[2]]$schema$fields, "name")
#' purrr::map_chr(package$resources[[2]]$schema$fields, "type")
read_resource <- function(resource_name, package) {
  # Get resource, includes check_package() and a number of other checks
  resource <- get_resource(resource_name, package)

  # Get paths, schema and fields
  paths <- resource$path
  schema <- get_schema(resource_name, package)
  fields <- schema$fields

  # Create locale with encoding, decimal_mark and grouping_mark
  d_chars <- purrr::map_chr(fields, ~ replace_null(.x$decimalChar, NA_character_))
  d_chars <- unique_sorted(d_chars)
  if (length(d_chars) == 0 | (length(d_chars) == 1 & d_chars[1] == ".")) {
    decimal_mark <- "." # Set default to "." if undefined or all set to "."
  } else {
    decimal_mark <- d_chars[1]
    warning(glue::glue(
      "Some fields define a non-default `decimalChar`. Only a global value is",
      "supported, so all number fields will be parsed with `{d_chars[1]}` as",
      "decimal mark.", .sep = " "
    ))
  }
  g_chars <- purrr::map_chr(fields, ~ replace_null(.x$groupChar, NA_character_))
  g_chars <- unique_sorted(g_chars)
  if (length(g_chars) == 0 | (length(g_chars) == 1 & g_chars[1] == "")) {
    grouping_mark <- "" # Set default to "" if undefined or all set to ""
  } else {
    grouping_mark <- g_chars[1]
    warning(glue::glue(
      "Some fields define a non-default `groupChar`. Only a global value is",
      "supported, so all number fields with this property will be parsed with",
      "`{g_chars[1]}` as grouping mark.", .sep = " "
    ))
  }
  locale <- readr::locale(
    encoding = replace_null(resource$encoding, "UTF-8"),
    decimal_mark = decimal_mark,
    grouping_mark = grouping_mark
  )

  # Create col_names: c("name1", "name2", ...)
  col_names <- purrr::map_chr(fields, ~ replace_null(.x$name, NA_character_))
  assertthat::assert_that(all(!is.na(col_names)),
    msg = glue::glue(
      "Field {which(is.na(col_names))} of resource `{resource_name}` must",
      "have the property `name`.", .sep = " "
    )
  )

  # Create col_types: list(<collector_character>, <collector_logical>, ...)
  col_types <- purrr::map(fields, function(x) {
    type <- replace_null(x$type, NA_character_)
    enum <- x$constraints$enum
    group_char <- ifelse(replace_null(x$groupChar, "") != "", TRUE, FALSE)
    bare_number <- ifelse(replace_null(x$bareNumber, "") != FALSE, TRUE, FALSE)
    format <- replace_null(x$format, "default") # Undefined => default
    convert_format <- function(format, translations) {
      format %>% stringr::str_replace_all(translations)
    }

    # Assign types and formats
    col_type <- switch(type,
      "string" = if (length(enum) > 0) {
          readr::col_factor(levels = enum)
        } else {
          readr::col_character()
        },
      "number" = if (length(enum) > 0) {
          readr::col_factor(levels = as.character(enum))
        } else if (group_char) {
          readr::col_number() # Supports grouping_mark
        } else if (bare_number) {
          readr::col_double() # Allows NaN, INF, -INF
        } else {
          readr::col_number() # Strips non-numeric chars + uses default grouping_mark
        },
      "integer" = if (length(enum) > 0) {
          readr::col_factor(levels = as.character(enum))
        } else if (bare_number) {
          readr::col_double() # Not col_integer() to avoid issues with big integers
        } else {
          readr::col_number() # Strips non-numeric chars
        },
      "boolean" = readr::col_logical(),
      "object" = readr::col_character(),
      "array" = readr::col_character(),
      "date" = readr::col_date(format = convert_format(format, c(
        "^default$" = "%Y-%m-%d", # ISO
        "^any$" = "%AD",          # YMD
        "^%x$" = "%m/%d/%y"       # Python strptime for %x
      ))),
      "time" = readr::col_time(format = convert_format(format, c(
        "^default$" = "%AT",      # H(MS)
        "^any$" = "%AT",          # H(MS)
        "^%X$" = "%H:%M:%S",      # HMS
        "%S.%f" = "%OS"           # Milli/microseconds
      ))),
      "datetime" = readr::col_datetime(format = convert_format(format, c(
        "^default$" = "",         # ISO (lenient)
        "^any$" = "",             # ISO (lenient)
        "%S.%f" = "%OS"           # Milli/microseconds
      ))),
      "year" = readr::col_date(format = "%Y"),
      "yearmonth" = readr::col_date(format = "%Y-%m"),
      "duration" = readr::col_character(),
      "geopoint" = readr::col_character(),
      "geojson" = readr::col_character(),
      "any" = readr::col_character()
    )
    # col_type will be NULL when type is undefined (NA_character) or an
    # unrecognized value (e.g. "datum"). Set those to col_guess()
    col_type <- replace_null(col_type, readr::col_guess())
    col_type
  })

  # Assign names: list("name1" = <collector_character>, "name2" = ...)
  names(col_types) <- col_names

  # Select CSV dialect, see https://specs.frictionlessdata.io/csv-dialect/
  dialect <- resource$dialect # Can be NULL

  # Read data directly
  if (resource$read_from == "df") {
    df <- dplyr::tibble(resource$data)

  # Read data from path(s)
  } else if (resource$read_from == "path") {
    dataframes <- list()
    for (i in seq_along(paths)) {
      data <- readr::read_delim(
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
        col_types = col_types,
        locale = locale,
        na = replace_null(schema$missingValues, ""),
        comment = replace_null(dialect$commentChar, ""),
        trim_ws = replace_null(dialect$skipInitialSpace, FALSE),
        # Skip header row when present
        skip = ifelse(replace_null(dialect$header, TRUE), 1, 0),
        skip_empty_rows = TRUE
      )
      dataframes[[i]] <- data
    }
    # Merge data frames for all paths
    df <- dplyr::bind_rows(dataframes)
  }

  return(df)
}
