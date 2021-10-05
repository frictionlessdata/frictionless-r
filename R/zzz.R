# HELPER FUNCTIONS

#' Replace value when NULL
#'
#' @param value Value to test.
#' @param replacement Replacement value when `value` is `NULL`.
#'
#' @return `value` when not `NULL`, otherwise `replacement`.
#'
#' @keywords internal
replace_null <- function(value, replacement) {
  if(!is.null(value)) { value } else { replacement }
}

#' Get unique vector values sorted by how often they occur
#'
#' @param x Vector, e.g. `c("a", "b", "b", "b", "a")`.
#'
#' @return Vector with unique values sorted by occurrence, e.g. `c("b", "a")`.
#'
#' @keywords interal
unique_sorted <- function(x) {
  stats::aggregate(x, by = list(x), FUN = length) %>%
    arrange(desc(x)) %>%
    pull("Group.1")
}

#' Check URL or path
#'
#' Check if a
#' [URL or path](https://specs.frictionlessdata.io/data-resource/#url-or-path)
#' is valid and prepend with directory to create a full path. Returns error when
#' no file can be found and optionally for unsafe absolute and relative parent
#' paths.
#'
#' @param url_or_path URL or path to a file.
#' @param directory Directory to prepend to path.
#' @param unsafe Allow unsafe absolute and relative parent paths.
#'
#' @return Full path or error.
#'
#' @keywords internal
#'
#' @importFrom assertthat assert_that
#' @importFrom glue glue
#' @importFrom httr http_error
check_path <- function(url_or_path, directory = NULL, unsafe = TRUE) {
  # url
  if (startsWith(url_or_path, "http")) {
    full_path <- url_or_path
    assert_that(
      !http_error(full_path),
      msg = glue("Can't find file at `{full_path}`.")
    )
  # path
  } else {
    assert_that(
      unsafe | !startsWith(url_or_path, "/"),
      msg = glue("{url_or_path} is an absolute path (`/`) which is unsafe.")
    )
    assert_that(
      unsafe | !startsWith(url_or_path, "../"),
      msg = glue(
        "{url_or_path} is a relative parent path (`../`) which is unsafe."
      )
    )
    # Prepend with directory if one is provided
    full_path <- ifelse(
      is.null(directory), url_or_path, paste(directory, url_or_path, sep = "/")
    )
    assert_that(
      file.exists(full_path),
      msg = glue("Can't find file at `{full_path}`.")
    )
  }
  return(full_path)
}
