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
  assertthat::assert_that(
    is.list(schema) & "fields" %in% names(schema) & is.list(schema["fields"]),
    msg = glue::glue("`schema` must be a list with property `fields`.")
  )
  fields <- schema$fields

  # Check fields have names
  field_names <- purrr::map_chr(fields, ~ replace_null(.x$name, NA_character_))
  assertthat::assert_that(
    all(!is.na(field_names)),
    msg = glue::glue(
      "All fields in `schema` must have property `name`.",
      "\u2139 Field(s) `{field_numbers_collapse}` don't have a name.",
      .sep = "\n",
      field_numbers_collapse = paste(which(is.na(field_names)), collapse = "`, `")
    )
  )

  # Check fields have valid types (a mix of valid types and undefined is ok)
  field_types <- purrr::map_chr(fields, ~ replace_null(.x$type, NA_character_))
  valid_types <- c(
    "string", "number", "integer", "boolean", "object", "array", "date", "time",
    "datetime", "year", "yearmonth", "duration", "geopoint", "geojson", "any",
    NA_character_
  )
  invalid_types <- setdiff(field_types, valid_types)
  assertthat::assert_that(
    all(is.na(field_types)) | length(invalid_types) == 0,
    msg = glue::glue(
      "All fields in `schema` must have valid `type`.",
      "Type `{invalid_types_collapse}` is invalid.",
      .sep = " ",
      invalid_types_collapse = paste(invalid_types, collapse = "`, `")
    )
  )

  # Check data when present
  if (!is.null(data)) {
    assertthat::assert_that(
      is.data.frame(data) &
        replace_null(dim(data)[1], 0) != 0 &
        replace_null(dim(data)[2], 0) != 0,
      msg = glue::glue(
        "`data` must be a data frame containing data."
      )
    )

    col_names <- colnames(data)
    assertthat::assert_that(
      identical(field_names, col_names),
      msg = glue::glue(
        "Field names in `schema` must match column names in data:",
        "\u2139 Field names: `{field_names_collapse}`",
        "\u2139 Column names: `{col_names_collapse}`",
        .sep = "\n",
        field_names_collapse = paste(field_names, collapse = "`, `"),
        col_names_collapse = paste(col_names, collapse = "`, `")
      )
    )
  } else {
    return(TRUE)
  }
}
