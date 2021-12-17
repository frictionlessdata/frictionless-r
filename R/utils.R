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
#' @param x Vector, e.g. `c("a", "b", "b", "b", "a")`.
#' @return Vector with unique values sorted by occurrence, e.g. `c("b", "a")`.
#' @family helper functions
#' @noRd
unique_sorted <- function(x) {
  dplyr::pull(
    dplyr::arrange(
      # Create data.frame with values ("Group.1") and how often they occur ("x")
      stats::aggregate(x, by = list(x), FUN = length),
      dplyr::desc(x)
    ),
    "Group.1"
  )
}

#' Clean list
#'
#' Removes all elements from a list that meet a criterion function, e.g.
#' `is.null(x)` for empty elements. Removal can be recursive to guarantee
#' elements are removed at any level.
#' Function is copied and adapted from [rlist::list.clean()] (MIT licensed), to
#' avoid requiring full `rlist` dependency.
#'
#' @param x List or vector.
#' @param fun Function returning `TRUE` for elements that should be removed.
#' @param recursive Whether list should be cleaned recursively.
#' @return Cleaned list.
#' @family helper functions
#' @noRd
list_clean <- function(x, fun = is.null, recursive = FALSE) {
  if (recursive) {
    x <- lapply(x, function(item) {
      if (is.list(item)) {
        list_clean(item, fun, recursive = TRUE)
      } else {
        item
      }
    })
  }
  "[<-"(x, vapply(x, fun, logical(1L)), NULL)
}

#' Check path or URL
#'
#' Check if a
#' [path or URL](https://specs.frictionlessdata.io/data-resource/#url-or-path)
#' is valid (and optionally safe) and prepend with directory to create an
#' absolute path or URL. Returns error when no file can be found.
#'
#' @param path Path or URL to a file.
#' @param directory Directory to prepend to path.
#' @param unsafe Allow `path` to be an unsafe absolute or relative parent path.
#' @return Absolute path or URL.
#' @family helper functions
#' @noRd
check_path <- function(path, directory = NULL, unsafe = TRUE) {
  # Check that (non-URL) path is safe and prepend with directory to make
  # absolute path (both optional)
  if (!startsWith(path, "http")) {
    assertthat::assert_that(
      unsafe | !startsWith(path, "/"),
      msg = glue::glue("`{path}` is an absolute path (`/`) which is unsafe.")
    )
    assertthat::assert_that(
      unsafe | !startsWith(path, "../"),
      msg = glue::glue(
        "`{path}` is a relative parent path (`../`) which is unsafe."
      )
    )
    if (!is.null(directory)) {
      path <- paste(directory, path, sep = "/")
    }
  }

  # Check existence of file at path
  if (startsWith(path, "http")) {
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

#' Read JSON at path or URL
#'
#' Reads JSON when provided property is a character (path or URL), otherwise
#' returns property.
#' @param x Any object or a path or URL to a file.
#' @param directory Directory to prepend to path.
#' @return `x` (unchanged) or loaded JSON at path or URL.
#' @family helper functions
#' @noRd
read_json <- function(x, directory) {
  if (is.character(x)) {
    x <- check_path(x, directory = directory, unsafe = FALSE)
    x <- jsonlite::fromJSON(x, simplifyDataFrame = FALSE)
  }
  return(x)
}
