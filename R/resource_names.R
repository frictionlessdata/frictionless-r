#' List Data Resources
#'
#' Lists the names of the Data Resources included in a Data Package.
#'
#' @inheritParams read_resource
#' @return Character vector with the Data Resource names.
#' @family read functions
#' @export
#' @examples
#' # Load the example Data Package
#' package <- example_package()
#'
#' # List the resources
#' resource_names(package)
resource_names <- function(package) {
  # Check package (and that all resource have a name)
  check_package(package)

  # Get resource names
  purrr::map_chr(package$resources, "name")
}
