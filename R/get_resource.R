#' Get a Data Resource
#'
#' Returns a [Data Resource](https://specs.frictionlessdata.io/data-resource/)
#' from a Data Package, i.e. the content of one of the described `resources`.
#'
#' @inheritParams read_resource
#' @return List object describing a Data Resource, with new property `full_path`
#'   containing absolute `path`s used for reading data.
#' @export
#' @examples
#' # Read a datapackage.json file
#' package <- read_package(
#'   system.file("extdata", "datapackage.json", package = "frictionless")
#' )
#'
#' # Get the Data Resource "observations"
#' resource <- get_resource("observations", package)
#' str(resource)
get_resource <- function(resource_name, package) {
  # Check package
  check_package(package)

  # Check resource
  resource_names_collapse <- paste(package$resource_names, collapse = ", ")
  assertthat::assert_that(
    resource_name %in% package$resource_names,
    msg = glue::glue(
      "Can't find resource `{resource_name}` in `{resource_names_collapse}`."
    )
  )

  # Get resource
  resource <- purrr::keep(package$resources, function(x) {
    (x$name == resource_name)
  })[[1]]

  # Check path(s) to file(s)
  # https://specs.frictionlessdata.io/data-resource/#data-location
  assertthat::assert_that(
    !is.null(resource$path) | !is.null(resource$data),
    msg = glue::glue(
      "Resource `{resource_name}` must have property `path` or `data."
    )
  )

  # Build full paths and attach as new property
  resource$full_path <- purrr::map_chr(
    resource$path, ~ check_path(.x, package$directory, unsafe = FALSE)
  )

  resource
}
