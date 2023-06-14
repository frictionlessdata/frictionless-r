# HELPER FUNCTIONS

#' Replace value when NULL
#'
#' @param x Value to test.
#' @param replacement Replacement value when `value` is `NULL`.
#' @return `value` when not `NULL`, otherwise `replacement`.
#' @family helper functions
#' @noRd
replace_null <- function(x, replacement) {
  if (!is.null(x)) {
    x
  } else {
    replacement
  }
}

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
  # Return empty char vector if all values in x where NA, resulting in NULL
  replace_null(values, character(0))
}

#' Clean list
#'
#' Removes all elements from a list that meet a criterion function, e.g.
#' `is.null(x)` for empty elements.
#' Removal can be recursive to guarantee elements are removed at any level.
#' Function is copied and adapted from [rlist::list.clean()] (MIT licensed), to
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

#' Check path or URL
#'
#' Check if a [path or
#' URL](https://specs.frictionlessdata.io/data-resource/#url-or-path) is valid
#' (and optionally safe) and prepend with directory to create an absolute path
#' or URL.
#' Returns error when no file can be found.
#'
#' @param path Path or URL to a file.
#' @param directory Directory to prepend to path.
#' @param safe Require `path` to be safe, i.e. no absolute or relative parent
#'   paths.
#' @return Absolute path or URL.
#' @family helper functions
#' @noRd
check_path <- function(path, directory = NULL, safe = FALSE) {
  # Check that (non-URL) path is safe and prepend with directory to make
  # absolute path (both optional)
  if (!is_url(path)) {
    assertthat::assert_that(
      !safe | !startsWith(path, "/"),
      msg = glue::glue("`{path}` is an absolute path (`/`) which is unsafe.")
    )
    assertthat::assert_that(
      !safe | !startsWith(path, "../"),
      msg = glue::glue(
        "`{path}` is a relative parent path (`../`) which is unsafe."
      )
    )
    if (!is.null(directory)) {
      path <- paste(directory, path, sep = "/")
    }
  }

  # Check existence of file at path
  if (is_url(path)) {
    assertthat::assert_that(
      !httr::http_error(path),
      msg = glue::glue("Can't find file at `{path}`.")
    )
  } else {
    assertthat::assert_that(
      file.exists(path),
      msg = glue::glue("Can't find file at `{path}`.")
    )
  }
  return(path)
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
  if (is.character(x)) {
    x <- check_path(x, directory = directory, safe = safe)
    if (grepl(".yaml$", x) | grepl(".yml$", x)) {
      x <- yaml::yaml.load_file(x)
    } else {
      # Default to jsonlite: better error messages for non .json files
      x <- jsonlite::fromJSON(x, simplifyDataFrame = FALSE, simplifyVector = TRUE)
    }
  }
  return(x)
}
