#' Create locale for a Data Resource
#'
#' Creates a [readr::locale()] object for a Data Resource, with the specified
#' `decimal_mark`, `grouping_mark` and `encoding`.
#'
#' @inheritParams read_resource
#' @return A [readr::locale()] object.
#' @family parse functions
#' @noRd
locale <- function(package, resource_name) {
  # Get resource, includes check_package()
  resource <- resource(package, resource_name)

  # Get fields
  schema <- get_schema(package, resource_name)
  fields <- schema$fields

  # Set decimal mark
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

  # Set grouping mark
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

  # Set encoding
  encoding <- resource$encoding %||% "UTF-8" # Set default to UTF-8
  if (!tolower(encoding) %in% tolower(iconvlist())) {
    cli::cli_warn(
      "Unknown encoding {.field {encoding}}. Reading file(s) with UTF-8
       encoding.",
      class = "frictionless_warning_resource_encoding_unknown"
    )
    encoding <- "UTF-8"
  }

  readr::locale(
    decimal_mark = decimal_mark,
    grouping_mark = grouping_mark,
    encoding = encoding
  )
}
