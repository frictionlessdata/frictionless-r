#' Create a Data Package
#'
#' Initiates a [Data Package](https://specs.frictionlessdata.io/data-package/)
#' object, either from scratch or from an existing list.
#' This Data Package object is a list with a `datapackage` class and the
#' following properties:
#' - All properties of the original `descriptor`.
#' - [`resources`](
#'   https://specs.frictionlessdata.io/data-package/#required-properties) (an
#'   empty list) if not present.
#' - `directory` (set to `"."` for the current directory) if not present.
#'   It is used as the base path to access resources with [read_resource()].
#'
#' The function will run [check_package()] on the created package to make sure
#' it is valid.
#'
#' @param descriptor List to be made into a Data Package object.
#'   If `NULL`, an empty Data Package object will be created from scratch.
#' @return Data Package object.
#' @family create functions
#' @export
#' @examples
#' # Create a Data Package
#' package <- create_package()
#' str(package)
create_package <- function(descriptor = NULL) {
  if (!is.null(descriptor) && !is.list(descriptor)) {
    cli::cli_abort(
      "{.arg descriptor} must be a list if provided.",
      class = "frictionless_error_descriptor_invalid"
    )
  }

  # Add properties
  descriptor$resources <- replace_null(descriptor$resources, list())
  descriptor$directory <- replace_null(descriptor$directory, ".") # Current dir

  # Add datapackage class
  if (!"datapackage" %in% class(descriptor)) {
    class(descriptor) <- c("datapackage", class(descriptor))
  }

  # Check that created package is valid
  check_package(descriptor)
  
  return(descriptor)
}
