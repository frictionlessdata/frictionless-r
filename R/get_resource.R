#' Get a Data Resource
#'
#' Returns a [Data Resource](https://specs.frictionlessdata.io/data-resource/)
#' from a Data Package, i.e. the content of one of the described `resources`.
#'
#' @inheritParams read_resource
#' @return List object describing a Data Resource.
#' @export
#' @examples
#' # Read a datapackage.json file
#' package <- read_package(
#'   system.file("extdata", "datapackage.json", package = "frictionless")
#' )
#'
#' # Get the Data Resource "observations"
#' resource <- get_resource(package, "observations")
#' str(resource)
get_resource <- function(package, resource_name) {
  # Check package
  check_package(package)

  # Get resource
  resource_names_collapse <- paste(package$resource_names, collapse = ", ")
  assertthat::assert_that(
    resource_name %in% package$resource_names,
    msg = glue::glue(
      "Can't find resource `{resource_name}` in `{resource_names_collapse}`."
    )
  )
  resource <- purrr::keep(package$resources, function(x) {
    (x$name == resource_name)
  })[[1]]
}
