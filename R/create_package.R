#' Create a Data Package
#'
#' Initiates a [Data Package](https://specs.frictionlessdata.io/data-package/)
#' object, either from scratch or from an existing list.
#' This Data Package object is a list with a `datapackage` class and the
#' following properties:
#' - All properties of the original `descriptor`.
#' - [`resources`](
#'   https://specs.frictionlessdata.io/data-package/#required-properties), if
#'   not provided.
#' - [`profile`](https://specs.frictionlessdata.io/data-package/#profile), if
#'   not provided.
#'   This property is set to `"tabular-data-package"`, since added resources
#'   will make the Data Package meet [Tabular Data Package](
#'   https://specs.frictionlessdata.io/tabular-data-package/) requirements.
#' - `directory`, if not provided.
#'   It is used as the base path to read resources with [read_resource()], with
#'   the current directory (`"."`) by default.
#'
#' @param descriptor List to be made into a Data Package object.
#'   If `NULL`, an empty Data Package object will be created from scratch.
#' @return Data Package object.
#'   Use [check_package()] to check if the output is a valid Data Package
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
