#' Remove a Data Package resource
#'
#' Removes a [Data Resource](https://specs.frictionlessdata.io/data-resource/)
#' from a Data Package, i.e. one of the described `resources` is removed.
#'
#' @param resource_name Name of the resource.
#' @param package Data Package object, see `read_package()`.
#'
#' @return Data Package object.
#'
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom glue glue
#' @importFrom purrr discard
#'
#' @examples
#' # Read datapackage.json file
#' package <- read_package(system.file("extdata", "datapackage.json", package = "datapackage"))
#'
#' # List resources
#' package$resource_names
#'
#' # Remove resource "observations"
#' package <- remove_resource("observations", package)
#'
#' # List resources
#' package$resource_names
remove_resource <- function(resource_name, package) {
  # Check resource is present
  resource <- get_resource(resource_name, package)

  # Remove resource
  package$resources <- discard(package$resources, function(x) {
    (x$name == resource_name)
  })

  # Remove resource_name
  resource_names <- package$resource_names
  package$resource_names <- resource_names[!(resource_names %in% resource_name)]

  package
}
