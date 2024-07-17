#' Create locale for a Data Resource
#'
#' Creates a [readr::locale()] object for a Data Resource, with the specified
#' `decimal_mark`, `grouping_mark` and `encoding`.
#'
#' @inheritParams read_resource
#' @return A [readr::locale()] object.
#' @family read functions
#' @noRd
locale <- function(package, resource_name) {
  schema <- get_schema(package, resource_name)
  fields <- schema$fields

  # Decimal mark
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

  # Grouping mark
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

  # Encoding
  encoding <- get_encoding(package, resource_name)

  readr::locale(
    decimal_mark = decimal_mark,
    grouping_mark = grouping_mark,
    encoding = encoding
  )
}
