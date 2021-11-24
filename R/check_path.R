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
#'
#' @return Absolute path or URL.
#'
#' @keywords internal
#'
#' @importFrom assertthat assert_that
#' @importFrom glue glue
#' @importFrom httr http_error
check_path <- function(path, directory = NULL, unsafe = TRUE) {

  # Check that (non-URL) path is safe and prepend with directory to make
  # absolute path (both optional)
  if (!startsWith(path, "http")) {
    assert_that(
      unsafe | !startsWith(path, "/"),
      msg = glue("{path} is an absolute path (`/`) which is unsafe.")
    )
    assert_that(
      unsafe | !startsWith(path, "../"),
      msg = glue(
        "{path} is a relative parent path (`../`) which is unsafe."
      )
    )
    if (!is.null(directory)) {
      path <- paste(directory, path, sep = "/")
    }
  }

  # Check existence of file at path
  if (startsWith(path, "http")) {
    assert_that(
      !http_error(path),
      msg = glue("Can't find file at `{path}`.")
    )
  } else {
    assert_that(
      file.exists(path),
      msg = glue("Can't find file at `{path}`.")
    )
  }
  return(path)
}