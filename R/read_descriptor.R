#' Read a datapackage.json file
#'
#' Loads information from a `datapackage.json` file, i.e. the
#' [Data Package descriptor](https://specs.frictionlessdata.io/data-package/#descriptor))
#' file that describes the Data Package metadata and its resources.
#'
#' @param descriptor_file Path to a `datapackage.json` file.
#'
#' @return List object containing all descriptor information and two new
#'   properties:
#'   - `resource_names`: vector with resource names.
#'   - `directory`: path of directory containing descriptor file, used as
#'      base path to read resources (`read_resources()`).
#'
#' @export
#'
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' descriptor <- read_descriptor(system.file("extdata", "datapackage.json", package = "datapackage"))
#' descriptor$resource_names
read_descriptor <- function(descriptor_file) {
  descriptor <- fromJSON(descriptor_file, simplifyDataFrame = FALSE)

  # Add resource_names
  descriptor$resource_names = map_chr(descriptor$resources, "name")

  # Add directory
  descriptor$directory <- dirname(descriptor_file)

  descriptor
}
