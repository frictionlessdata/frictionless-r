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
  # Process path
  if (!is_url(path)) {
    # Check absolute path
    if (safe && startsWith(path, "/")) {
      cli::cli_abort(
        c(
          "{.arg path} must be a safe path.",
          "x" = "{.path {path}} is an absolute path starting with {.val /}
                 which is unsafe."
        ),
        class = "frictionless_error_path_unsafe_absolute"
      )
    }

    # Check relative path
    if (safe && startsWith(path, "../")) {
      cli::cli_abort(
        c(
          "{.arg path} must be a safe path.",
          "x" = "{.path {path}} is a relative parent path starting with
                 {.val ../} which is unsafe."
        ),
        class = "frictionless_error_path_unsafe_relative"
      )
    }

    # Prepend with directory (which can be a URL)
    if (!is_url(path) && !is.null(directory)) {
      path <- file.path(directory, path)
    }
  }

  # Check existence of file at path
  if (is_url(path)) {
    if (httr::http_error(path)) {
      cli::cli_abort(
        "Can't find file at {.url {path}}.",
        class = "frictionless_error_url_not_found"
      )
    }
  } else {
    if (!file.exists(path)) {
      cli::cli_abort(
        "Can't find file at {.path {path}}.",
        class = "frictionless_error_path_not_found"
      )
    }
  }
  return(path)
}
