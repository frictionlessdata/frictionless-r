#' Read data from the path of a Data Resource
#'
#' @inheritParams read_resource
#' @return [tibble::tibble()] data frame
#' @family read functions
#' @noRd
read_from_path <- function(package, resource_name, col_select) {
  # Get resource, includes check_package()
  resource <- get_resource(package, resource_name)

  # Get paths, schema and fields
  paths <- resource$path
  schema <- get_schema(package, resource_name)
  fields <- schema$fields
  field_names <- purrr::map_chr(fields, ~ purrr::pluck(.x, "name"))

  # Reassign col_select to avoid lazy eval
  col_select <- col_select

  # Check all selected columns appear in schema
  if (!all(col_select %in% field_names)) {
    col_select_missing <- col_select[!col_select %in% field_names]
    cli::cli_abort(
      c(
        "Can't find column{?s} {.val {col_select_missing}} in field names.",
        "i" = "Field name{?s}: {.val {field_names}}."
      ),
      class = "frictionless_error_colselect_mismatch"
    )
  }

  # Get locale with decimal_mark, grouping_mark and encoding
  locale <- locale(package, resource_name)

  # Get col_types
  col_types <- cols(schema)

  # Get dialect (can be NULL)
  dialect <- read_descriptor(resource$dialect, package$directory, safe = TRUE)
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

  # Read data with read_delim for each path (returns tibble)
  purrr::map_df(
    paths,
    function(path) {
      readr::read_delim(
        file = path,
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
  )
}
