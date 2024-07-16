#' Create column types for reading a Data Resource.
#'
#' Create a list of readr column types for reading a [Data
#' Resource](https://specs.frictionlessdata.io/data-resource/) (in a Data
#' Package) based on the schema's fields of the resource.
#' @param package Data Package.
#' @param resource_name Name of the resource.
#' @return A [readr::cols()] object.
#' @family helper functions
#' @noRd
#' @examples
#' # Load the example Data Package
#' package <- example_package()
#'
#' # Create col types for the resource "observations"
#' frictionless:::create_col_types(package, "observations")
create_col_types <- function(package, resource_name) {
  schema <- get_schema(package, resource_name)
  fields <- schema$fields
  field_names <- purrr::map_chr(fields, ~ purrr::pluck(.x, "name"))
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
#' @param x Field from resource's schema.
#' @return A readr column type.
#' @family helper functions
#' @noRd
#' @examples
#' # Load the example Data Package
#' package <- example_package()
#' schema <- get_schema(package, "observations")
#' fields <- schema$fields
#' # Create col type for first field (string)
#' frictionless:::create_col_type(fields[[1]])
#' # Create col type for third field (datetime)
#' frictionless:::create_col_type(fields[[3]])
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
