#' Check Table Schema object
#'
#' Check if an object is a list object describing a Table Schema and
#' (optionally) compare against a provided data frame.
#'
#' @param schema List object describing a Table Schema.
#' @param df A data frame against which the Table Schema must be compared.
#' @return `TRUE` or error.
#' @noRd
check_schema <- function(schema, df = NULL) {
  # Check schema is list with property fields
  assertthat::assert_that(
    is.list(schema) & "fields" %in% names(schema) & is.list(schema["fields"]),
    msg = glue::glue("`schema` must be a list with property `fields`.")
  )
  fields <- schema$fields

  # Check fields have names
  field_names <- purrr::map_chr(fields, ~ replace_null(.x$name, NA_character_))
  field_numbers_collapse <- paste(which(is.na(field_names)), collapse = "`, `")
  assertthat::assert_that(
    all(!is.na(field_names)),
    msg = glue::glue(
      "All fields in `schema` must have property `name`.",
      "* Field(s) `{field_numbers_collapse}` don't have a name.", .sep = "\n"
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
      "Type `{invalid_types}` is invalid.", .sep = " "
    )
  )

  # Check df when present
  if (!is.null(df)) {
    assertthat::assert_that(
      is.data.frame(df) &
      replace_null(dim(df)[2], 0) != 0,
      msg = glue::glue(
        "`df` must be a data frame with columns."
      )
    )

    field_names_collapse <- paste(field_names, collapse = "`, `")
    col_names <- colnames(df)
    col_names_collapse <- paste(col_names, collapse = "`, `")
    assertthat::assert_that(
      identical(field_names, col_names),
      msg = glue::glue(
        "Field names in `schema` must match column names in `df`:",
        "* Field names: {field_names_collapse}",
        "* Column names: {col_names_collapse}",
        .sep = "\n"
      )
    )
  } else {
    return(TRUE)
  }
}
