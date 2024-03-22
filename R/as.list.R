#' Convert a Data Package to a list
#'
#' Converts a Data Package to a list, removing any custom classes including
#' `datapackage`.
#' To convert a list back to a Data Package, use [create_package()].
#'
#' @inheritParams read_package
#' @return Data Package object converted to a list.
#' @family coercion functions
#' @export
#' @examples
#' # Load the example Data Package
#' package <- example_package
#'
#' # Convert the Data Package to a list
#' package_as_list <- as.list(package)
#'
#' # Convert the list back to a Data Package
#' create_package(package_as_list)
as.list.datapackage <- function(package) {
  class(package) <- "list"

  return(package)
}
