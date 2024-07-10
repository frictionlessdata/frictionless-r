# HELPER FUNCTIONS

#' Get unique vector values sorted by how often they occur
#'
#' @param x Vector, e.g. `c("a", "b", "b", "b", "c", "a")`.
#' @return Vector with unique values sorted by most to least occurring,
#'   e.g. `c("b", "a", "c")`.
#' @family helper functions
#' @noRd
unique_sorted <- function(x) {
  # Create table, sort on occurrence, return values (names)
  # c a b
  # 1 2 3
  values <- names(sort(table(x), decreasing = TRUE))
  # Return empty char vector if all values in x are NA, resulting in NULL
  values %||% character(0)
}

#' Clean list
#'
#' Removes all elements from a list that meet a criterion function, e.g.
#' `is.null(x)` for empty elements.
#' Removal can be recursive to guarantee elements are removed at any level.
#' Function is copied and adapted from `rlist::list.clean()` (MIT licensed), to
#' avoid requiring full `rlist` dependency.
#'
#' @param x List or vector.
#' @param fun Function returning `TRUE` for elements that should be removed.
#' @param recursive Whether list should be cleaned recursively.
#' @return Cleaned list.
#' @family helper functions
#' @noRd
clean_list <- function(x, fun = is.null, recursive = FALSE) {
  if (recursive) {
    x <- lapply(x, function(item) {
      if (is.list(item)) {
        clean_list(item, fun, recursive = TRUE)
      } else {
        item
      }
    })
  }
  "[<-"(x, vapply(x, fun, logical(1L)), NULL)
}

#' Check if path is URL
#'
#' @param path Path.
#' @return `TRUE` if `path` is a http(s) or (s)ftp URL, otherwise `FALSE`.
#' @family helper functions
#' @noRd
is_url <- function(path) {
  grepl("^(http|https|ftp|ftps|sftp):\\/\\/", path)
}

#' Read descriptor
#'
#' Returns descriptor `x` as is, or attempts to read JSON/YAML from path or URL.
#'
#' @inheritParams check_path
#' @return `x` (unchanged) or loaded JSON/YAML at path or URL.
#' @family helper functions
#' @noRd
read_descriptor <- function(x, directory = NULL, safe = FALSE) {
  # Return object
  if (!is.character(x)) {
    return(x)
  }

  # Read file
  x <- check_path(x, directory = directory, safe = safe)
  if (grepl("\\.yaml$", x) || grepl("\\.yml$", x)) {
    yaml::yaml.load_file(x)
  } else {
    # Default to jsonlite: better error messages for non .json files
    jsonlite::fromJSON(x, simplifyDataFrame = FALSE, simplifyVector = TRUE)
  }
}

#' Read encoding
#'
#' Returns the encoding of a [Data
#' Resource](https://specs.frictionlessdata.io/data-resource/) (in a Data
#' Package), or sets it to UTF-8 if unknown.
#' A warning is returned if the given encoding is unknown.
#'
#' @param resource Resource.
#' @return Encoding of resource, or UTF-8 if unknown.
#' @family helper functions
#' @noRd
get_encoding <- function(resource) {
  encoding <- resource$encoding %||% "UTF-8" # Set default to UTF-8
  if (!tolower(encoding) %in% tolower(iconvlist())) {
    cli::cli_warn(
      "Unknown encoding {.field {encoding}}. Reading file(s) with UTF-8
         encoding.",
      class = "frictionless_warning_resource_encoding_unknown"
    )
    encoding <- "UTF-8"
  }
  return(encoding)
}

#' Create locale for reading a Data Resource
#'
#' Create a [readr::locale()] object for reading a [Data
#' Resource](https://specs.frictionlessdata.io/data-resource/) (in a Data
#' Package) with the correct encoding, decimal and grouping mark.
#'
#' @param resource Resource.
#' @param fields Fields from schema of the resource.
#' @family helper functions
#' @noRd
create_locale <- function(resource, fields) {
  encoding <- get_encoding(resource)
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
  readr::locale(
    encoding = encoding,
    decimal_mark = decimal_mark,
    grouping_mark = grouping_mark
  )
}

#' Create column types for reading a Data Resource.
#'
#' Create a list of readr column types for reading a [Data
#' Resource](https://specs.frictionlessdata.io/data-resource/) (in a Data
#' Package) based on the schema's fields of the resource.
#' @param fields Fields from schema of the resource.
#' @param field_names Names of the fields.
#' @return A [readr::cols()] object.
#' @family helper functions
#' @noRd
create_col_types <- function(fields, field_names) {
  col_types <- purrr::map(fields, create_col_type)
  # Assign names: list("name1" = <collector_character>, "name2" = ...)
  names(col_types) <- field_names
  col_types
}

#' Create column type for reading a specific field of a Data Resource.
#'
#' Create a readr column type for reading a [Data
#' Resource](https://specs.frictionlessdata.io/data-resource/) (in a Data
#' Package) based on the schema's field.
#'
#' @param x Field from schema of the resource.
#' @return A readr column type.
#' @family helper functions
#' @noRd
create_col_type <- function(x) {
  type <- x$type %||% NA_character_
  enum <- x$constraints$enum
  group_char <- if (x$groupChar %||% "" != "") TRUE else FALSE
  bare_number <- if (x$bareNumber %||% "" != FALSE) TRUE else FALSE
  format <- x$format %||% "default" # Undefined => default

  # Assign types and formats
  col_type <- switch(type,
                     "string" = col_string(enum),
                     "number" = col_number(enum, group_char, bare_number),
                     "integer" = col_integer(enum, bare_number),
                     "boolean" = readr::col_logical(),
                     "object" = readr::col_character(),
                     "array" = readr::col_character(),
                     "date" = col_date(format),
                     "time" = col_time(format),
                     "datetime" = col_datetime(format),
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
}

#' Create column type for reading a string field of a Data Resource.
#'
#' Create a readr column type for reading a string field.
#'
#' @param enum Enumerated values, if any.
#' @return A [readr::col_factor()] or a [readr::col_character()] object.
#' @family helper functions
#' @noRd
col_string <- function(enum) {
  if (length(enum) > 0) {
    readr::col_factor(levels = enum)
  } else {
    readr::col_character()
  }
}

#' Create column type for reading a number field of a Data Resource.
#'
#' Create a readr column type for reading a number field.
#'
#' @param enum Enumerated values, if any.
#' @param group_char Whether to use [readr::col_number()], which supports
#'   grouping marks.
#' @param bare_number Whether to use [readr::col_double()], which allows NaN,
#'   INF and -INF.
#' @return A [readr::col_factor()], a [readr::col_number()], or a
#'   [readr::col_double] object.
#' @family helper functions
#' @noRd
col_number <- function(enum, group_char, bare_number) {
  if (length(enum) > 0) {
    readr::col_factor(levels = as.character(enum))
  } else if (group_char) {
    readr::col_number() # Supports grouping_mark
  } else if (bare_number) {
    readr::col_double() # Allows NaN, INF, -INF
  } else {
    readr::col_number() # Strips non-num. chars, uses default grouping_mark
  }
}

#' Create column type for reading an integer field of a Data Resource.
#'
#' Create a readr column type for reading an integer field.
#'
#' @param enum Enumerated values, if any.
#' @param bare_number Whether to use [readr::col_double()]. This is done to
#'  avoid issues with big integers.
#' @return A [readr::col_factor()], a [readr::col_double] object or a
#'   [readr::col_number()] object.
#' @family helper functions
#' @noRd
col_integer <- function(enum, bare_number) {
  if (length(enum) > 0) {
    readr::col_factor(levels = as.character(enum))
  } else if (bare_number) {
    readr::col_double() # Not col_integer() to avoid big integers issues
  } else {
    readr::col_number() # Strips non-numeric chars
  }
}

#' Create column type for reading a date field of a Data Resource.
#'
#' Create a readr column type for reading a date field.
#'
#' @param format Datetime format.
#' @return A [readr::col_date()] object.
#' @family helper functions
#' @noRd
col_date <- function(format) {
  readr::col_date(format = switch(format,
                                  "default" = "%Y-%m-%d", # ISO
                                  "any" = "%AD", # YMD
                                  "%x" = "%m/%d/%y", # Python strptime for %x
                                  format # Default
  ))
}

#' Create column type for reading a time field of a Data Resource.
#'
#' Create a readr column type for reading a time field.
#'
#' @param format Datetime format.
#' @return A [readr::col_time()] object.
#' @family helper functions
#' @noRd
col_time <- function(format) {
  readr::col_time(format = switch(format,
                                  "default" = "%AT", # H(MS)
                                  "any" = "%AT", # H(MS)
                                  "%X" = "%H:%M:%S", # HMS
                                  sub("%S.%f", "%OS", format) # Default, use %OS for milli/microseconds
  ))
}

#' Create column type for reading a datetime field of a Data Resource.
#'
#' Create a readr column type for reading a datetime field.
#'
#' @param format Datetime format.
#' @return A [readr::col_datetime()] object.
#' @family helper functions
#' @noRd
col_datetime <- function(format) {
  readr::col_datetime(format = switch(format,
                                      "default" = "", # ISO (lenient)
                                      "any" = "", # ISO (lenient)
                                      sub("%S.%f", "%OS", format) # Default, use %OS for milli/microseconds
  ))
}

#' Read data from a path with user defined specifications.
#'
#' @param x Path to file.
#' @param dialect Dialect of the data in `path`.
#' @param field_names Names of the schema's fields.
#' @param col_types Column types, e.g. as created by `create_col_types()`.
#' @param col_select Columns in data to read.
#' @param schema Schema of the resource.
#' @param locale Locale, e.g. as created by `create_locale()`.
#' @return Data frame.
#' @family helper functions
#' @noRd
read_from_path <- function(x,
                          dialect,
                          field_names,
                          col_types,
                          col_select,
                          schema,
                          locale) {
  escape_backslash <- if (dialect$escapeChar %||% "not set" == "\\") {
    TRUE
  } else {
    FALSE
  }
  escape_double <- if (dialect$escapeChar %||% "not set" == "\\") {
    # If escapeChar is set, set doubleQuote to FALSE (mutually exclusive)
    FALSE
  } else {
    dialect$doubleQuote %||% TRUE
  }
  skip <- if (dialect$header %||% TRUE) 1 else 0
  readr::read_delim(
    file = x,
    delim = dialect$delimiter %||% ",",
    quote = dialect$quoteChar %||% "\"",
    escape_backslash = escape_backslash,
    escape_double = escape_double,
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
    skip = skip,
    skip_empty_rows = TRUE
  )
}
