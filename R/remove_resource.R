#' Remove a Data Resource
#'
#' Removes a [Data Resource](https://specs.frictionlessdata.io/data-resource/)
#' from a Data Package, i.e. one of the described `resources` is removed.
#'
#' @inheritParams read_resource
#' @return List object describing a Data Package.
#' @export
#' @examples
#' # Read a datapackage.json file
#' package <- read_package(system.file("extdata", "datapackage.json", package = "frictionless"))
#'
#' # List the resource names
#' package$resource_names
#'
#' # Remove the resource "observations"
#' package <- remove_resource("observations", package)
#'
#' # List the resource names ("observations" removed)
#' package$resource_names
remove_resource <- function(resource_name, package) {
  # Check resource is present
  resource <- get_resource(resource_name, package)

  # Remove resource
  package$resources <- purrr::discard(package$resources, function(x) {
    (x$name == resource_name)
  })

  # Remove resource_name
  resource_names <- package$resource_names
  package$resource_names <- resource_names[!(resource_names %in% resource_name)]

  package
}
