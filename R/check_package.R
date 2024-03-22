#' Check a Data Package object
#'
#' Check if an object is a Data Package object with the required properties.
#'
#' @inheritParams read_resource
#' @return `TRUE` or error.
#' @family check functions
#' @export
#' @examples
#' # Load the example Data Package
#' package <- example_package
#'
#' # Check if the Data Package is valid
#' check_package(package)
check_package <- function(package) {
  general_message <- "{.arg package} must be a Data Package object."
  tip_message <- paste(
    "Create a valid Data Package object with {.fun read_package} or ",
    "{.fun create_package}."
  )

  # Check package is a list
  if (!is.list(package)) {
    cli::cli_abort(
      c(
        general_message,
        "x" = "{.arg package} is not a list.",
        "i" = tip_message
      ),
      class = "frictionless_error_package_invalid"
    )
  }

  # Check package has resources (list)
  if (!is.list(package$resources)) {
    cli::cli_abort(
      c(
        general_message,
        "x" = "{.arg package} is missing a {.field resources} property or it is
               not a list.",
        "i" = tip_message
      ),
      class = "frictionless_error_package_invalid"
    )
  }

  # Check package has directory (character)
  if (!is.character(package$directory)) {
    cli::cli_abort(
      c(
        general_message,
        "x" = "{.arg package} is missing a {.field directory} property or it is
               not a character.",
        "i" = tip_message
      ),
      class = "frictionless_error_package_invalid"
    )
  }

  # Check all resources (if any) have a name
  if (purrr::some(package$resources, ~ is.null(.x$name))) {
    cli::cli_abort(
      "All {.field resources} in {.arg package} must have a {.field name}
       property.",
      class = "frictionless_error_resources_without_name"
    )
  }

  # Warn if class is missing
  if (!"datapackage" %in% class(package)) {
    cli::cli_warn(
      c(
        general_message,
        "x" = "{.arg package} is missing a {.val datapackage} class.",
        "i" = tip_message
      ),
      class = "frictionless_warning_package_without_class"
    )
  }

  return(TRUE)
}
