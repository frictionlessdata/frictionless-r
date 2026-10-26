#' Check a path or URL
#'
#' Check if a [path or URL](
#' https://datapackage.org/standard/data-resource/#path-or-data) is valid (and
#' optionally safe) and prepend with directory to create an absolute path or
#' URL.
#' Returns error when no file can be found.
#'
#' @param path Path or URL to a file.
#' @param directory Directory to prepend to path.
#' @param safe Require `path` to be safe, i.e. no absolute, relative parent or
#'   hidden directory paths.
#' @returns Absolute path or URL or an error.
#' @family check functions
#' @noRd
check_path <- function(path, directory = NULL, safe = FALSE) {
  # Process path
  if (!is_url(path)) {
    path <- fs::path(path)

    # Check absolute path
    if (safe && fs::is_absolute_path(path)) {
      cli::cli_abort(
        c(
          "{.arg path} must be a safe path.",
          "x" = "{.path {path}} is an absolute path which is unsafe."
        ),
        class = "frictionless_error_path_unsafe_absolute"
      )
    }

    # Check relative path
    if (safe) {
      parts <- unlist(fs::path_split(path), use.names = FALSE)
      if (any(parts == "..")) {
        cli::cli_abort(
          c(
            "{.arg path} must be a safe path.",
            "x" = "{.path {path}} is a relative parent path which is unsafe."
          ),
          class = "frictionless_error_path_unsafe_relative"
        )
      }
    }

    # Check hidden directory path (containing ".any_name/")
    if (safe) {
      parts <- unlist(fs::path_split(path), use.names = FALSE)
      if (any(grepl("^\\.[^/\\\\]+$", parts))) {
        cli::cli_abort(
          c(
            "{.arg path} must be a safe path.",
            "x" = "{.path {path}} contains a hidden folder (starting with
                   {.val .}) which is unsafe."
          ),
          class = "frictionless_error_path_unsafe_hidden"
        )
      }
    }

    # Prepend with directory (which can be a URL)
    if (!is_url(path) && !is.null(directory)) {
      path <- fs::path(directory, path)
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
    if (!fs::file_exists(path)) {
      cli::cli_abort(
        "Can't find file at {.path {path}}.",
        class = "frictionless_error_path_not_found"
      )
    }
  }

  return(path)
}
