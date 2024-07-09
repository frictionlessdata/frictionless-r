# HELPER FUNCTIONS

#' Get unique vector values sorted by how often they occur
#'
#' @param x Vector, e.g. `c("a", "b", "b", "b", "c", "a")`.
#' @return Vector with unique values sorted by most to least occurring,
#'   e.g. `c("b", "a", "c")`.
#' @family helper functions
#' @noRd
unique_sorted <- function(x) {
  # Create table, sort on occurrence, return values (names)
  # c a b
  # 1 2 3
  values <- names(sort(table(x), decreasing = TRUE))
  # Return empty char vector if all values in x are NA, resulting in NULL
  values %||% character(0)
}

#' Clean list
#'
#' Removes all elements from a list that meet a criterion function, e.g.
#' `is.null(x)` for empty elements.
#' Removal can be recursive to guarantee elements are removed at any level.
#' Function is copied and adapted from `rlist::list.clean()` (MIT licensed), to
#' avoid requiring full `rlist` dependency.
#'
#' @param x List or vector.
#' @param fun Function returning `TRUE` for elements that should be removed.
#' @param recursive Whether list should be cleaned recursively.
#' @return Cleaned list.
#' @family helper functions
#' @noRd
clean_list <- function(x, fun = is.null, recursive = FALSE) {
  if (recursive) {
    x <- lapply(x, function(item) {
      if (is.list(item)) {
        clean_list(item, fun, recursive = TRUE)
      } else {
        item
      }
    })
  }
  "[<-"(x, vapply(x, fun, logical(1L)), NULL)
}

#' Check if path is URL
#'
#' @param path Path.
#' @return `TRUE` if `path` is a http(s) or (s)ftp URL, otherwise `FALSE`.
#' @family helper functions
#' @noRd
is_url <- function(path) {
  grepl("^(http|https|ftp|ftps|sftp):\\/\\/", path)
}

#' Read descriptor
#'
#' Returns descriptor `x` as is, or attempts to read JSON/YAML from path or URL.
#'
#' @inheritParams check_path
#' @return `x` (unchanged) or loaded JSON/YAML at path or URL.
#' @family helper functions
#' @noRd
read_descriptor <- function(x, directory = NULL, safe = FALSE) {
  # Return object
  if (!is.character(x)) {
    return(x)
  }

  # Read file
  x <- check_path(x, directory = directory, safe = safe)
  if (grepl("\\.yaml$", x) || grepl("\\.yml$", x)) {
    yaml::yaml.load_file(x)
  } else {
    # Default to jsonlite: better error messages for non .json files
    jsonlite::fromJSON(x, simplifyDataFrame = FALSE, simplifyVector = TRUE)
  }
}


#' Get the field names from a schema
#'
#' @param schema
#'
#' @return A character vector of the values for `name` in `schema$fields`
#' @family helper functions
#' @noRd
#' @examples
#' df <- data.frame("space" = c("the", "final", "frontier"),
#'                  "enterprise" = c("c", "d", "e"))
#' create_schema(df) %>% get_fields_names()
get_fields_names <- function(schema) {
  # For every list element within `$fields`
  schema_fields <- purrr::chuck(schema, "fields")
  # Get the value for `name`
  purrr::map_chr(schema_fields, ~purrr::pluck(.x, "name"))
}
