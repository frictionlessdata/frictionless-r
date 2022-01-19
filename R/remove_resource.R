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
#' # List the resource names
#' package$resource_names
#'
#' # Remove the resource "observations"
#' package <- remove_resource(package, "observations")
#'
#' # List the resource names ("observations" removed)
#' package$resource_names
remove_resource <- function(package, resource_name) {
  # Check resource is present, includes check_package()
  resource <- get_resource(package, resource_name)

  # Remove resource
  package$resources <- purrr::discard(package$resources, function(x) {
    (x$name == resource_name)
  })

  # Remove resource_name
  resource_names <- package$resource_names
  package$resource_names <- resource_names[!(resource_names %in% resource_name)]

  package
}
