#' Read data from a Data Package resource
#'
#' Loads data from a Data Package **resource** into a tibble (a Tidyverse
#' data.frame).
#'
#' @param descriptor Descriptor object (see `read_descriptor()`).
#' @param resource_name Name of the resource to load data from.
#'
#' @return tibble with the resource data.
#'
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#' descriptor <- read_descriptor(system.file("extdata", "datapackage.json", package = "datapackage"))
#' df <- read_resource(descriptor, "deployments")
#' }
read_resource <- function(descriptor, resource_name) {
  # Verify that resource exists
  assert_that(resource_name %in% descriptor$resource_names,
    msg = paste0("Can't find resource \"", resource_name, "\"")
  )
  resource_name
}
