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
