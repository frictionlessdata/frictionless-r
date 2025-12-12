#' Create a Data Package
#'
#' Initiates a Data Package object, either from scratch or from an existing
#' list.
#' This Data Package object is a list with the following characteristics:
#' - All properties of the original `descriptor`.
#' - A `resources` property, set to an empty list if undefined.
#' - A `directory` attribute, set to `"."` for the current directory if
#'   undefined.
#'   It is used as the base path to access resources with [read_resource()].
#' - A `datapackage` subclass.
#'
#' See `vignette("data-package")` to learn how this function implements the
#' Data Package standard.
#' [check_package()] is automatically called on the created package to make sure
#' it is valid.
#'
#' @param descriptor List to be made into a Data Package object.
#'   If undefined, an empty Data Package will be created from scratch.
#' @return A Data Package object.
#' @family create functions
#' @export
#' @examples
#' # Create a Data Package
#' package <- create_package()
#'
#' package
#'
#' # See the structure of the (empty) Data Package
#' str(package)
create_package <- function(descriptor = NULL) {
  if (!is.null(descriptor) && !is.list(descriptor)) {
    cli::cli_abort(
      "{.arg descriptor} must be a list if provided.",
      class = "frictionless_error_descriptor_invalid"
    )
  }

  # Add resources property (also creates descriptor if NULL)
  descriptor$resources <- descriptor$resources %||% list()

  # Add directory attribute
  attr(descriptor, "directory") <- attr(descriptor, "directory") %||% "."

  # Add datapackage class
  if (!"datapackage" %in% class(descriptor)) {
    class(descriptor) <- c("datapackage", class(descriptor))
  }

  # Check that created package is valid
  check_package(descriptor)

  return(descriptor)
}
