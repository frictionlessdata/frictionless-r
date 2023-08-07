#' Read data from a Data Resource into a tibble data frame
#'
#' Reads data from a [Data
#' Resource](https://specs.frictionlessdata.io/data-resource/) (in a Data
#' Package) into a tibble (a Tidyverse data frame).
#' The resource must be a [Tabular Data
#' Resource](https://specs.frictionlessdata.io/tabular-data-resource/).
#' The function uses [readr::read_delim()] to read CSV files, passing the
#' resource properties `path`, CSV dialect, column names, data types, etc.
#' Column names are taken from the provided Table Schema (`schema`), not from
#' the header in the CSV file(s).
#'
#' @param package List describing a Data Package, created with [read_package()]
#'   or [create_package()].
#' @param resource_name Name of the Data Resource.
#' @param col_select Character vector of the columns to include in the result,
#'   in the order provided.
#'   Selecting columns can improve read speed.
#' @return A [tibble()] data frame with the Data Resource's tabular data.
#'   If there are parsing problems, a warning will alert you.
#'   You can retrieve the full details by calling [problems()] on your data
#'   frame.
#' @family read functions
#' @export
#' @section Resource properties:
#' The [Data Resource
#' properties](https://specs.frictionlessdata.io/data-resource/) are handled as
#' follows:
#'
#' ## Path
#'
#' [`path`](https://specs.frictionlessdata.io/data-resource/#data-location) is
#' required.
#' It can be a local path or URL, which must resolve.
#' Absolute path (`/`) and relative parent path (`../`) are forbidden to avoid
#' security vulnerabilities.
#'
#' When multiple paths are provided (`"path": [ "myfile1.csv", "myfile2.csv"]`)
#' then data are merged into a single data frame, in the order in which the
#' paths are listed.
#'
#' ## Data
#'
#' If `path` is not present, the function will attempt to read data from the
#' `data` property.
#' **`schema` will be ignored**.
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
#' `encoding` (e.g. `windows-1252`) is
#' [required](https://specs.frictionlessdata.io/data-resource/#optional-properties)
#' if the resource file(s) is not encoded as UTF-8.
#' The returned data frame will always be UTF-8.
#'
#' ## CSV Dialect
#'
#' `dialect` properties are
#' [required](https://specs.frictionlessdata.io/csv-dialect/#specification) if
#' the resource file(s) deviate from the default CSV settings (see below).
#' It can either be a JSON object or a path or URL referencing a JSON object.
#' Only deviating properties need to be specified, e.g. a tab delimited file
#' without a header row needs:
#' ```json
#' "dialect": {"delimiter": "\t", "header": "false"}
#' ```
#'
#' These are the CSV dialect properties.
#' Some are ignored by the function:
#' - `delimiter`: default `,`.
#' - `lineTerminator`: ignored, line terminator characters `LF` and `CRLF` are
#'   interpreted automatically by [readr::read_delim()], while `CR` (used by
#'   Classic Mac OS, final release 2001) is not supported.
#' - `doubleQuote`: default `true`.
#' - `quoteChar`: default `"`.
#' - `escapeChar`: anything but `\` is ignored and it will set `doubleQuote` to
#'   `false` as these fields are mutually exclusive.
#'   You can thus not escape with `\"` and `""` in the same file.
#' - `nullSequence`: ignored, use `missingValues`.
#' - `skipInitialSpace`: default `false`.
#' - `header`: default `true`.
#' - `commentChar`: not set by default.
#' - `caseSensitiveHeader`: ignored, header is not used for column names, see
#'   Schema.
#' - `csvddfVersion`: ignored.
#'
#' ## File compression
#'
#' Resource file(s) with `path` ending in `.gz`, `.bz2`, `.xz`, or `.zip` are
#' automatically decompressed using default [readr::read_delim()]
#' functionality.
#' Only `.gz` files can be read directly from URL `path`s.
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
#' Schema](https://specs.frictionlessdata.io/table-schema/) specification.
#' It can either be a JSON object or a path or URL referencing a JSON object.
#'
#' - Field `name`s are used as column headers.
#' - Field `type`s are use as column types (see further).
#' - [`missingValues`](https://specs.frictionlessdata.io/table-schema/#missing-values)
#'   are used to interpret as `NA`, with `""` as default.
#'
#' ## Field types
#'
#' Field `type` is used to set the column type, as follows:
#'
#' - [string](https://specs.frictionlessdata.io/table-schema/#string) as
#'   `character`; or `factor` when `enum` is present.
#'   `format` is ignored.
#' - [number](https://specs.frictionlessdata.io/table-schema/#number) as
#'   `double`; or `factor` when `enum` is present.
#'   Use `bareNumber: false` to ignore whitespace and non-numeric characters.
#'   `decimalChar` (`.` by default) and `groupChar` (undefined by default) can
#'   be defined, but the most occurring value will be used as a global value for
#'   all number fields of that resource.
#' - [integer](https://specs.frictionlessdata.io/table-schema/#integer) as
#'   `double` (not integer, to avoid issues with big numbers); or `factor` when
#'   `enum` is present.
#'   Use `bareNumber: false` to ignore whitespace and non-numeric characters.
#' - [boolean](https://specs.frictionlessdata.io/table-schema/#boolean) as
#'   `logical`.
#'   Non-default `trueValues/falseValues` are not supported.
#' - [object](https://specs.frictionlessdata.io/table-schema/#object) as
#'   `character`.
#' - [array](https://specs.frictionlessdata.io/table-schema/#array) as
#'   `character`.
#' - [date](https://specs.frictionlessdata.io/table-schema/#date) as `date`.
#'   Supports `format`, with values `default` (ISO date), `any` (guess `ymd`)
#'   and [Python/C strptime](https://docs.python.org/2/library/datetime.html#strftime-strptime-behavior)
#'   patterns, such as `%a, %d %B %Y` for `Sat, 23 November 2013`.
#'   `%x` is `%m/%d/%y`.
#'   `%j`, `%U`, `%w` and `%W` are not supported.
#' - [time](https://specs.frictionlessdata.io/table-schema/#time) as
#'   [hms::hms()].
#'   Supports `format`, with values `default` (ISO time), `any` (guess `hms`)
#'   and [Python/C strptime](https://docs.python.org/2/library/datetime.html#strftime-strptime-behavior)
#'   patterns, such as `%I%p%M:%S.%f%z` for `8AM30:00.300+0200`.
#' - [datetime](https://specs.frictionlessdata.io/table-schema/#datetime) as
#'   `POSIXct`.
#'   Supports `format`, with values `default` (ISO datetime), `any`
#'   (ISO datetime) and the same patterns as for `date` and `time`.
#'   `%c` is not supported.
#' - [year](https://specs.frictionlessdata.io/table-schema/#year) as `date`,
#'   with `01` for month and day.
#' - [yearmonth](https://specs.frictionlessdata.io/table-schema/#yearmonth) as
#'   `date`, with `01` for day.
#' - [duration](https://specs.frictionlessdata.io/table-schema/#duration) as
#'   `character`.
#'   Can be parsed afterwards with [lubridate::duration()].
#' - [geopoint](https://specs.frictionlessdata.io/table-schema/#geopoint) as
#'   `character`.
#' - [geojson](https://specs.frictionlessdata.io/table-schema/#geojson) as
#'   `character`.
#' - [any](https://specs.frictionlessdata.io/table-schema/#any) as `character`.
#' - Any other value is not allowed.
#' - Type is guessed when not provided.
#' @examples
#' # Read a datapackage.json file
#' package <- read_package(
#'   system.file("extdata", "datapackage.json", package = "frictionless")
#' )
#'
#' # List resources
#' resources(package)
#'
#' # Read data from the resource "observations"
#' read_resource(package, "observations")
#'
#' # The above tibble is merged from 2 files listed in the resource path
#' package$resources[[2]]$path
#'
#' # The column names and types are derived from the resource schema
#' purrr::map_chr(package$resources[[2]]$schema$fields, "name")
#' purrr::map_chr(package$resources[[2]]$schema$fields, "type")
#'
#' # Read data from the resource "deployments" with column selection
#' read_resource(package, "deployments", col_select = c("latitude", "longitude"))
read_resource <- function(package, resource_name, col_select = NULL) {
  # Get resource, includes check_package()
  resource <- get_resource(package, resource_name)

  # Get paths, schema and fields
  paths <- resource$path
  schema <- get_schema(package, resource_name)
  fields <- schema$fields
  field_names <- purrr::map_chr(fields, ~ purrr::pluck(.x, "name"))

  # Check all selected columns appear in schema
  assertthat::assert_that(
    all(col_select %in% field_names),
    msg = glue::glue(
      "Can't find column(s) {missing_col_select_collapse} in field names.",
      "\u2139 Field names: {field_names_collapse}",
      missing_col_select_collapse = glue::glue_collapse(
        glue::backtick(col_select[!col_select %in% field_names]),
        sep = ", "
      ),
      field_names_collapse = glue::glue_collapse(
        glue::backtick(field_names),
        sep = ", "
      ),
      .sep = "\n"
    )
  )

  # Create locale with encoding, decimal_mark and grouping_mark
  encoding <- replace_null(resource$encoding, "UTF-8") # Set default to UTF-8
  if (!tolower(encoding) %in% tolower(iconvlist())) {
    warning(glue::glue(
      "Unknown encoding `{encoding}`. Reading file(s) with UTF-8 encoding."
    ))
    encoding <- "UTF-8"
  }
  d_chars <- purrr::map_chr(
    fields, ~ replace_null(.x$decimalChar, NA_character_)
  )
  d_chars <- unique_sorted(d_chars)
  if (length(d_chars) == 0 || (length(d_chars) == 1 && d_chars[1] == ".")) {
    decimal_mark <- "." # Set default to "." if undefined or all set to "."
  } else {
    decimal_mark <- d_chars[1]
    warning(glue::glue(
      "Some fields define a non-default `decimalChar`.",
      "Parsing all number fields with `{d_chars[1]}` as decimal mark.",
      .sep = " "
    ))
  }
  g_chars <- purrr::map_chr(fields, ~ replace_null(.x$groupChar, NA_character_))
  g_chars <- unique_sorted(g_chars)
  if (length(g_chars) == 0 || (length(g_chars) == 1 && g_chars[1] == "")) {
    grouping_mark <- "" # Set default to "" if undefined or all set to ""
  } else {
    grouping_mark <- g_chars[1]
    warning(glue::glue(
      "Some fields define a non-default `groupChar`.",
      "Parsing all number fields with `{g_chars[1]}` as grouping mark.",
      .sep = " "
    ))
  }
  locale <- readr::locale(
    encoding = encoding,
    decimal_mark = decimal_mark,
    grouping_mark = grouping_mark
  )

  # Create col_names: c("name1", "name2", ...)
  col_names <- purrr::map_chr(fields, ~ replace_null(.x$name, NA_character_))
  assertthat::assert_that(all(!is.na(col_names)),
    msg = glue::glue(
      "Field {which(is.na(col_names))} of resource `{resource_name}` must",
      "have the property `name`.",
      .sep = " "
    )
  )

  # Create col_types: list(<collector_character>, <collector_logical>, ...)
  col_types <- purrr::map(fields, function(x) {
    type <- replace_null(x$type, NA_character_)
    enum <- x$constraints$enum
    group_char <- ifelse(replace_null(x$groupChar, "") != "", TRUE, FALSE)
    bare_number <- ifelse(replace_null(x$bareNumber, "") != FALSE, TRUE, FALSE)
    format <- replace_null(x$format, "default") # Undefined => default

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
        readr::col_number() # Strips non-num. chars, uses default grouping_mark
      },
      "integer" = if (length(enum) > 0) {
        readr::col_factor(levels = as.character(enum))
      } else if (bare_number) {
        readr::col_double() # Not col_integer() to avoid big integers issues
      } else {
        readr::col_number() # Strips non-numeric chars
      },
      "boolean" = readr::col_logical(),
      "object" = readr::col_character(),
      "array" = readr::col_character(),
      "date" = readr::col_date(format = switch(format,
        "default" = "%Y-%m-%d", # ISO
        "any" = "%AD", # YMD
        "%x" = "%m/%d/%y", # Python strptime for %x
        format # Default
      )),
      "time" = readr::col_time(format = switch(format,
        "default" = "%AT", # H(MS)
        "any" = "%AT", # H(MS)
        "%X" = "%H:%M:%S", # HMS
        gsub("%S.%f", "%OS", format) # Default, use %OS for milli/microseconds
      )),
      "datetime" = readr::col_datetime(format = switch(format,
        "default" = "", # ISO (lenient)
        "any" = "", # ISO (lenient)
        gsub("%S.%f", "%OS", format) # Default, use %OS for milli/microseconds
      )),
      "year" = readr::col_date(format = "%Y"),
      "yearmonth" = readr::col_date(format = "%Y-%m"),
      "duration" = readr::col_character(),
      "geopoint" = readr::col_character(),
      "geojson" = readr::col_character(),
      "any" = readr::col_character()
    )
    # col_type will be NULL when type is undefined (NA_character_) or an
    # unrecognized value (e.g. "datum", but will be blocked by check_schema()).
    # Set those to col_guess().
    col_type <- replace_null(col_type, readr::col_guess())
    col_type
  })

  # Assign names: list("name1" = <collector_character>, "name2" = ...)
  names(col_types) <- col_names

  # Select CSV dialect, see https://specs.frictionlessdata.io/csv-dialect/
  # Note that dialect can be NULL
  dialect <- read_descriptor(resource$dialect, package$directory, safe = TRUE)

  # Read data directly
  if (resource$read_from == "df") {
    df <- dplyr::as_tibble(resource$data)
    data_col_names <- colnames(df)
  # Read data from data
  } else if (resource$read_from == "data") {
    df <- dplyr::as_tibble(do.call(rbind.data.frame, resource$data))
    data_col_names <- colnames(df)
  # Read data from path(s)
  } else if (resource$read_from == "path" || resource$read_from == "url") {
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
          # If escapeChar is set, set doubleQuote to FALSE (mutually exclusive)
          replace_null(dialect$escapeChar, "not set") == "\\",
          FALSE,
          replace_null(dialect$doubleQuote, TRUE)
        ),
        col_names = col_names,
        col_types = col_types,
        # Use rlang {{}} to avoid `col_select` to be interpreted as the name of
        # a column, see https://rlang.r-lib.org/reference/topic-data-mask.html
        col_select = {{col_select}},
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
    # Read header from first file
    data_col_names <-
      readr::read_lines(file = paths[1], n_max = 1) %>%
      I() %>%
      readr::read_delim(delim = replace_null(dialect$delimiter, ","),
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
                        locale = locale,
                        na = replace_null(schema$missingValues, ""),
                        comment = replace_null(dialect$commentChar, ""),
                        trim_ws = replace_null(dialect$skipInitialSpace, FALSE),
                        col_names = FALSE,
                        show_col_types = FALSE) %>%
      dplyr::slice_head(n = 1) %>%
      unlist(use.names = FALSE)
  }
  # compare df header to schema
  assertthat::assert_that(
    identical(tolower(col_names), tolower(data_col_names)),
    msg = glue::glue(
      "Field names in `schema` must match column names in data:",
      "\u2139 Field names: `{field_names_collapse}`",
      "\u2139 Column names in data: `{data_col_names_collapse}`",
      .sep = "\n",
      field_names_collapse = glue::glue_collapse(col_names, sep = ", "),
      data_col_names_collapse = glue::glue_collapse(data_col_names, sep = ", ")
    ))

  return(df)
}
