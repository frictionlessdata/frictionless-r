#' Create locale for a Data Resource
#'
#' Creates a [readr::locale()] object for a Data Resource, with the specified
#' `decimal_mark`, `grouping_mark` and `encoding`.
#'
#' @inheritParams read_resource
#' @returns A [readr::locale()] object.
#' @family parse functions
#' @noRd
locale <- function(package, resource_name) {
  # Get resource, includes check_package()
  resource <- resource(package, resource_name)

  # Get fields
  schema <- schema(package, resource_name)
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
      "Some fields define a {.field groupChar}. Parsing all number
       and integer fields with {.val {g_chars[1]}} as grouping mark.",
      class = "frictionless_warning_fields_groupchar_different"
    )
  }

  # Decimal mark and grouping mark must be different
  if (decimal_mark == grouping_mark) {
    # Error message slightly different if decimal_mark is default (".") or not
    if (decimal_mark == ".") {
      cli::cli_abort(c(
        "Grouping mark must be different than decimal mark.",
        "i" = "Some fields define a {.field groupChar} ({.val {grouping_mark}})
        that is the same as {.field decimalChar} ({.val {decimal_mark}},
        <the default>)."
        ),
      class = "frictionless_error_fields_decimalchar_groupchar_same_default"
      )
    } else {
      cli::cli_abort(c(
        "Grouping mark must be different than decimal mark.",
        "i" = "Some fields define a {.field groupChar} ({.val {grouping_mark}})
        that is the same as {.field decimalChar} ({.val {decimal_mark}})."
        ),
      class = "frictionless_error_fields_decimalchar_groupchar_same"
      )
    }
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
