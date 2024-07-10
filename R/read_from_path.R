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
    col_select = if (is.null(col_select)) NULL else {{ col_select }},
    locale = locale,
    na = schema$missingValues %||% "",
    comment = dialect$commentChar %||% "",
    trim_ws = dialect$skipInitialSpace %||% FALSE,
    # Skip header row when present
    skip = skip,
    skip_empty_rows = TRUE
  )
}
