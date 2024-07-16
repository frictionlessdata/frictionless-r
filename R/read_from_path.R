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
#' @examples
#' # Load the example Data Package
#' package <- example_package()
#'
#' # Get the path to a Resource
#' path <- frictionless:::get_resource(package, "observations")$path[1]
#' dialect <- NULL
#' schema <- get_schema(package, "observations")
#' # From https://github.com/frictionlessdata/frictionless-r/pull/237/files
#' # To be replaced with get_fields_names().
#' fields <- purrr::chuck(schema, "fields")
#' field_names <- purrr::map_chr(fields, ~ purrr::pluck(.x, "name"))
#' col_types <- frictionless:::create_col_types(package, "observations")
#' col_select <- NULL
#' locale <- frictionless:::create_locale(package, "observations")
#' frictionless:::read_from_path(
#'   path,
#'   dialect,
#'   field_names,
#'   col_types,
#'   col_select,
#'   schema,
#'   locale
#' )
#'
#' # Select only some columns
#' colselect <- c("observation_id", "deployment_id")
#' frictionless:::read_from_path(
#'   path,
#'   dialect,
#'   field_names,
#'   col_types,
#'   col_select,
#'   schema,
#'   locale
#' )
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
  col_select <- col_select
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
    col_select = {{ col_select }},
    locale = locale,
    na = schema$missingValues %||% "",
    comment = dialect$commentChar %||% "",
    trim_ws = dialect$skipInitialSpace %||% FALSE,
    # Skip header row when present
    skip = skip,
    skip_empty_rows = TRUE
  )
}
