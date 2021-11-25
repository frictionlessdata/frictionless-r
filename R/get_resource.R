#' Get a Data Resource
#'
#' Returns a [Data Resource](https://specs.frictionlessdata.io/data-resource/)
#' from a Data Package, i.e. the content of one of the described `resources`.
#'
#' @param resource_name Name of the resource.
#' @param package Data Package object, see `read_package()`.
#'
#' @return List object describing a Data Resource.
#'
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom glue glue
#' @importFrom purrr keep
#'
#' @examples
#' # Read datapackage.json file
#' package <- read_package(system.file("extdata", "datapackage.json", package = "frictionless"))
#'
#' # Get resource "observations"
#' resource <- get_resource("observations", package)
#' str(resource)
get_resource <- function(resource_name, package) {
  # Check package
  check_package(package)

  # Get resource
  resource_names_collapse <- paste(package$resource_names, collapse = ", ")
  assert_that(
    resource_name %in% package$resource_names,
    msg = glue(
      "Can't find resource `{resource_name}` in `{resource_names_collapse}`."
    )
  )
  resource <- keep(package$resources, function(x) {
    (x$name == resource_name)
  })[[1]]

  resource
}
