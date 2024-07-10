#' Remove a Data Resource
#'
#' Removes a [Data Resource](https://specs.frictionlessdata.io/data-resource/)
#' from a Data Package, i.e. it removes one of the described `resources`.
#'
#' @inheritParams read_resource
#' @return Provided `package` with one fewer resource.
#' @family edit functions
#' @export
#' @examples
#' # Load the example Data Package
#' package <- example_package
#'
#' # List the resources
#' resources(package)
#'
#' # Remove the resource "observations"
#' package <- remove_resource(package, "observations")
#'
#' # List the resources ("observations" removed)
#' resources(package)
remove_resource <- function(package, resource_name) {
  # Check resource is present, includes check_package()
  resource <- get_resource(package, resource_name)

  # Remove resource
  package$resources <- purrr::discard(package$resources, function(x) {
    (x$name == resource_name)
  })

  return(package)
}
