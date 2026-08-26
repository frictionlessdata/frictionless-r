#' Create a column specification for a Table Schema
#'
#' Creates a [readr::cols()] for all fields in a Table Schema.
#'
#' @inheritParams check_schema
#' @returns A [readr::cols()] object.
#' @family parse functions
#' @noRd
cols <- function(schema) {
  # Check schema
  check_schema(schema)

  # Get fields
  fields <- schema$fields
  field_names <- purrr::map_chr(fields, ~ purrr::pluck(.x, "name"))

  # Create col_types
  col_types <- purrr::map(fields, field_to_col)
  # Add names: list("name1" = <collector_character>, "name2" = ...)
  names(col_types) <- field_names

  # Replicate structure of readr::col_spec
  structure(
    list(
      cols = col_types
    ),
    class = "col_spec"
  )
}

#' Create a column specification for a field in Table Schema
#'
#' Creates a column specification for a specific field in a Table Schema.
#'
#' @param field Field in a Table schema.
#' @returns A readr collector.
#' @family parse functions
#' @noRd
field_to_col <- function(field) {
  # Get field properties
  type <- field$type %||% NA_character_
  enum <- unlist(field$constraints$enum)
  group_char <- if (field$groupChar %||% "" != "") TRUE else FALSE
  bare_number <- if (field$bareNumber %||% "" != FALSE) TRUE else FALSE
  format <- field$format %||% "default" # Undefined => default

  # Add types and formats
  col_type <- switch(
    type,
    "string" = col_string(enum),
    "number" = col_number(enum, group_char, bare_number),
    "integer" = col_integer(enum, group_char, bare_number),
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
  # Don't guess, but set to character.
  col_type <- col_type %||% readr::col_character()
  col_type
}
