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
#' @examples
#' # Load the example Data Package
#' package <- example_package
#'
#' # Create locale of resource "observations"
#' frictionless:::create_locale(package, resource_name = "observations")
create_locale <- function(package, resource_name) {
  encoding <- get_encoding(package, resource_name)
  schema <- get_schema(package, resource_name)
  # From https://github.com/frictionlessdata/frictionless-r/pull/237/files
  # To be replaced with get_fields_names().
  # For every list element within `$fields`
  fields <- purrr::chuck(schema, "fields")

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
