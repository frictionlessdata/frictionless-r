#' List Data Resources
#'
#' List the names of the Data Resources included in a Data Package
#'
#' @param package List describing a Data Package.
#' @return Character vector with the resource names.
#' @family read functions
#' @export
#' @examples
#' # Load the example Data Package
#' package <- example_package
#'
#' # List the resource names
#' resources(package)
resources <- function(package) {
  check_package(package)
  package$resource_names
}
