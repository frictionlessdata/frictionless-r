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
#' @param package Data Package object, created with [read_package()] or
#'   [create_package()].
#' @param resource_name Name of the Data Resource.
#' @param col_select Character vector of the columns to include in the result,
#'   in the order provided.
#'   Selecting columns can improve read speed.
#' @return [tibble::tibble()] data frame with the Data Resource's tabular data.
#'   If there are parsing problems, a warning will alert you.
#'   You can retrieve the full details by calling [problems()] on your data
#'   frame.
#' @family read functions
#' @export
#' @section Resource properties:
#'
#' See `vignette("data-resource")`.
#'
#' ## CSV Dialect
#'
#' See `vignette("table-dialect")`.
#'
#' @section Table schema properties:
#'
#' See `vignette("table-schema")`.
#'
#' @examples
#' # Read a datapackage.json file
#' package <- read_package(
#'   system.file("extdata", "datapackage.json", package = "frictionless")
#' )
#'
#' package
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
  if (!all(col_select %in% field_names)) {
    col_select_missing <- col_select[!col_select %in% field_names]
    cli::cli_abort(
      c(
        "Can't find column{?s} {.val {col_select_missing}} in field names.",
        "i" = "Field name{?s}: {.val {field_names}}."
      ),
      class = "frictionless_error_colselect_mismatch"
    )
  }

  # Create locale with encoding, decimal_mark and grouping_mark
  encoding <- resource$encoding %||% "UTF-8" # Set default to UTF-8
  if (!tolower(encoding) %in% tolower(iconvlist())) {
    cli::cli_warn(
      "Unknown encoding {.field {encoding}}. Reading file(s) with UTF-8
       encoding.",
      class = "frictionless_warning_resource_encoding_unknown"
    )
    encoding <- "UTF-8"
  }
  d_chars <- purrr::map_chr(fields, ~ .x$decimalChar %||% NA_character_)
  d_chars <- unique_sorted(d_chars)
  if (length(d_chars) == 0 || (length(d_chars) == 1 && d_chars[1] == ".")) {
    decimal_mark <- "." # Set default to "." if undefined or all set to "."
  } else {
    decimal_mark <- d_chars[1]
    cli::cli_warn(
      "Some fields define a non-default {.field decimalChar}. Parsing all number
       fields with {.val {d_chars[1]}} as decimal mark.",
      class = "frictionless_warning_fields_decimalchar_different"
    )
  }
  g_chars <- purrr::map_chr(fields, ~ .x$groupChar %||% NA_character_)
  g_chars <- unique_sorted(g_chars)
  if (length(g_chars) == 0 || (length(g_chars) == 1 && g_chars[1] == "")) {
    grouping_mark <- "" # Set default to "" if undefined or all set to ""
  } else {
    grouping_mark <- g_chars[1]
    cli::cli_warn(
      "Some fields define a non-default {.field groupChar}. Parsing all number
       fields with {.val {g_chars[1]}} as grouping mark.",
      class = "frictionless_warning_fields_groupchar_different"
    )
  }
  locale <- readr::locale(
    encoding = encoding,
    decimal_mark = decimal_mark,
    grouping_mark = grouping_mark
  )

  # Create col_types: list(<collector_character>, <collector_logical>, ...)
  col_types <- purrr::map(fields, function(x) {
    type <- x$type %||% NA_character_
    enum <- x$constraints$enum
    group_char <- if (x$groupChar %||% "" != "") TRUE else FALSE
    bare_number <- if (x$bareNumber %||% "" != FALSE) TRUE else FALSE
    format <- x$format %||% "default" # Undefined => default

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
        sub("%S.%f", "%OS", format) # Default, use %OS for milli/microseconds
      )),
      "datetime" = readr::col_datetime(format = switch(format,
        "default" = "", # ISO (lenient)
        "any" = "", # ISO (lenient)
        sub("%S.%f", "%OS", format) # Default, use %OS for milli/microseconds
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
    col_type <- col_type %||% readr::col_guess()
    col_type
  })

  # Assign names: list("name1" = <collector_character>, "name2" = ...)
  names(col_types) <- field_names

  # Select CSV dialect, see https://specs.frictionlessdata.io/csv-dialect/
  # Note that dialect can be NULL
  dialect <- read_descriptor(resource$dialect, package$directory, safe = TRUE)

  # Read data directly
  if (resource$read_from == "df") {
    df <- dplyr::as_tibble(resource$data)

  # Read data from data
  } else if (resource$read_from == "data") {
    df <- dplyr::as_tibble(do.call(rbind.data.frame, resource$data))

  # Read data from path(s)
  } else if (resource$read_from == "path" || resource$read_from == "url") {
    dataframes <- list()
    for (i in seq_along(paths)) {
      data <- readr::read_delim(
        file = paths[i],
        delim = dialect$delimiter %||% ",",
        quote = dialect$quoteChar %||% "\"",
        escape_backslash = if (dialect$escapeChar %||% "not set" == "\\") {
          TRUE
        } else {
          FALSE
        },
        escape_double = if (dialect$escapeChar %||% "not set" == "\\") {
          # If escapeChar is set, set doubleQuote to FALSE (mutually exclusive)
          FALSE
        } else {
          dialect$doubleQuote %||% TRUE
        },
        col_names = field_names,
        col_types = col_types,
        # Use rlang {{}} to avoid `col_select` to be interpreted as the name of
        # a column, see https://rlang.r-lib.org/reference/topic-data-mask.html
        col_select = {{col_select}},
        locale = locale,
        na = schema$missingValues %||% "",
        comment = dialect$commentChar %||% "",
        trim_ws = dialect$skipInitialSpace %||% FALSE,
        # Skip header row when present
        skip = if (dialect$header %||% TRUE) 1 else 0,
        skip_empty_rows = TRUE
      )
      dataframes[[i]] <- data
    }
    # Merge data frames for all paths
    df <- dplyr::bind_rows(dataframes)
  }

  return(df)
}
