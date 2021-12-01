# HELPER FUNCTIONS

#' Replace value when NULL
#'
#' @param x Value to test.
#' @param replacement Replacement value when `value` is `NULL`.
#' @return `value` when not `NULL`, otherwise `replacement`.
#' @noRd
replace_null <- function(x, replacement) {
  if (!is.null(x)) { x } else { replacement }
}

#' Get unique vector values sorted by how often they occur
#'
#' @param x Vector, e.g. `c("a", "b", "b", "b", "a")`.
#' @return Vector with unique values sorted by occurrence, e.g. `c("b", "a")`.
#' @noRd
unique_sorted <- function(x) {
  stats::aggregate(x, by = list(x), FUN = length) %>%
    dplyr::arrange(dplyr::desc(x)) %>%
    dplyr::pull("Group.1")
}

#' Check package object
#'
#' Check if a package object is a list object of class `datapackage`, with the
#' required properties.
#'
#' @param package List object describing a Data Package.
#' @return `TRUE` or error.
#' @noRd
check_package <- function(package) {
  assertthat::assert_that(
    all(c("datapackage", "list") %in% class(package)) &
    all(c("resources", "resource_names", "directory") %in% names(package)) &
    is.list(package$resources) &
    is.character(package$resource_names) &
    is.character(package$directory),
    msg = glue::glue(
      "`package` must be a list object of class `datapackage` created with",
      "`read_package()` or `create_package()`.", .sep = " "
    )
  )
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
#' @noRd
check_path <- function(path, directory = NULL, unsafe = TRUE) {

  # Check that (non-URL) path is safe and prepend with directory to make
  # absolute path (both optional)
  if (!startsWith(path, "http")) {
    assertthat::assert_that(
      unsafe | !startsWith(path, "/"),
      msg = glue::glue("{path} is an absolute path (`/`) which is unsafe.")
    )
    assertthat::assert_that(
      unsafe | !startsWith(path, "../"),
      msg = glue::glue(
        "{path} is a relative parent path (`../`) which is unsafe."
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
#' @noRd
read_json <- function(x, directory) {
  if (is.character(x)) {
    x <- check_path(x, directory = directory, unsafe = FALSE)
    x <- jsonlite::fromJSON(x, simplifyDataFrame = FALSE)
  }
  return(x)
}
