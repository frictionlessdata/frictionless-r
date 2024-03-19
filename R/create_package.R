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
#' - [`profile`](https://specs.frictionlessdata.io/data-package/#profile) (set
#'   to `"tabular-data-package"`) if not present.
#' - `directory` (set to `"."` for the current directory) if not present.
#'   It is used as the base path to access resources with [read_resource()].
#'
#' @param descriptor List to be made into a Data Package object.
#'   If `NULL`, an empty Data Package object will be created from scratch.
#' @return Data Package object.
#'   Use [check_package()] to check if it is a valid Data Package
#'   object.
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
  descriptor$profile <- replace_null(descriptor$profile, "tabular-data-package")
  descriptor$resources <- replace_null(descriptor$resources, list())
  descriptor$directory <- replace_null(descriptor$directory, ".") # Current dir

  # Add datapackage class
  if (!"datapackage" %in% class(descriptor)) {
    class(descriptor) <- c("datapackage", class(descriptor))
  }
  descriptor
}
