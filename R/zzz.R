# SUPPORT FUNCTIONS

#' Check URL or path
#'
#' Check if a
#' [URL or path](https://specs.frictionlessdata.io/data-resource/#url-or-path)
#' is valid and prepend with directory to create a full path. Returns error for
#' unsafe absolute and relative parent paths, or when no file can be found.
#'
#' @param url_or_path URL or path to a file.
#' @param directory Directory to prepend to path.
#'
#' @return Full path or error.
#'
#' @internal
#'
#' @importFrom assertthat assert_that
#' @importFrom glue glue
#' @importFrom httr http_error
check_path <- function(url_or_path, directory = ".") {
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
      !startsWith(url_or_path, "/"),
      msg = glue("{url_or_path} is an absolute path (`/`) which is unsafe.")
    )
    assert_that(
      !startsWith(url_or_path, "../"),
      msg = glue(
        "{url_or_path} is a relative parent path (`../`) which is unsafe."
      )
    )
    full_path <- paste(directory, url_or_path, sep = "/")
    assert_that(
      file.exists(full_path),
      msg = glue("Can't find file at `{full_path}`.")
    )
  }
  return(full_path)
}
