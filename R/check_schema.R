#' Check Table Schema object
#'
#' Check if an object is a list describing a Table Schema and (optionally)
#' compare against a provided data frame.
#'
#' @param schema List describing a Table Schema.
#' @param data A data frame against which the Table Schema must be compared.
#' @return `TRUE` or error.
#' @family check functions
#' @noRd
check_schema <- function(schema, data = NULL) {
  # Check schema is list with property fields
  if (
    !is.list(schema) ||
    !"fields" %in% names(schema) ||
    !is.list(schema["fields"])
  ) {
    cli::cli_abort(
      "{.arg schema} must be a list with a {.field fields} property.",
      class = "frictionless_error_schema_invalid"
    )
  }
  fields <- schema$fields

  # Check fields have names
  field_names <- purrr::map_chr(fields, ~ replace_null(.x$name, NA_character_))
  fields_without_name <- as.character(which(is.na(field_names)))
  if (any(is.na(field_names))) {
    cli::cli_abort(
      c(
        "All fields in {.arg schema} must have a {.field name} property.",
        "x" = "Field{?s} {fields_without_name} {?doesn't/don't} have a
               {.field name}."
      ),
      class = "frictionless_error_fields_without_name"
    )
  }

  # Check fields have valid types (a mix of valid types and undefined is ok)
  field_types <- purrr::map_chr(fields, ~ replace_null(.x$type, NA_character_))
  valid_types <- c(
    "string", "number", "integer", "boolean", "object", "array", "date", "time",
    "datetime", "year", "yearmonth", "duration", "geopoint", "geojson", "any",
    NA_character_
  )
  invalid_types <- setdiff(field_types, valid_types)
  if (length(invalid_types) > 0) {
    cli::cli_abort(
      c(
        "All fields in {.arg schema} must have a valid {.field type}.",
        "x" = "Type{?s} {.val {invalid_types}} {?is/are} invalid."
      ),
      class = "frictionless_error_fields_type_invalid"
    )
  }

  # Check data when present
  if (!is.null(data)) {
    check_data(data)

    col_names <- colnames(data)
    assertthat::assert_that(
      identical(field_names, col_names),
      msg = glue::glue(
        "Field names in `schema` must match column names in data:",
        "\u2139 Field names: {field_names_collapse}",
        "\u2139 Column names: {col_names_collapse}",
        .sep = "\n",
        field_names_collapse = glue::glue_collapse(
          glue::backtick(field_names),
          sep = ", "
        ),
        col_names_collapse = glue::glue_collapse(
          glue::backtick(col_names),
          sep = ", "
        )
      )
    )
  } else {
    return(TRUE)
  }
}
