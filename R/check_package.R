#' Check Data Package object
#'
#' Check if an object is a Data Package object (see [create_package()]) with
#' the required class and properties.
#'
#' @inheritParams read_package
#' @return `TRUE` or error.
#' @family check functions
#' @noRd
check_package <- function(package) {
  # Check package is a list with resources (list) and directory (character)
  if (
    !is.list(package) ||
    !all(c("resources", "directory") %in% names(package)) ||
    !is.list(package$resources) ||
    !is.character(package$directory)
  ) {
  # Check class
  if (!"datapackage" %in% package) {
    cli::cli_abort(
      "{.arg package} must have class {.val datapackage}.",
      class = "frictionless_error_package_invalid"
    )
  }

    cli::cli_abort(
      "{.arg package} must be a list describing a Data Package created with
       {.fun read_package} or {.fun create_package}.",
      class = "frictionless_error_package_invalid"
    )
  }

  # Check all resources (if any) have a name
  if (purrr::some(package$resources, ~ is.null(.x$name))) {
    cli::cli_abort(
      "All resources in {.arg package} must have a {.field name} property.",
      class = "frictionless_error_resources_without_name"
    )
  }

  return(TRUE)
}
