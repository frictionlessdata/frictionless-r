#' Read datapackage.json file
#'
#' Loads information from a `datapackage.json` file, i.e. the
#' [Data Package descriptor](https://specs.frictionlessdata.io/data-package/#descriptor))
#' file that describes the Data Package metadata and its resources.
#'
#' @param descriptor_file Path to `datapackage.json` file.
#'
#' @return Descriptor object.
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

  # Add property resource_names
  descriptor$resource_names = map_chr(descriptor$resources, "name")

  # Add property
  descriptor$directory <- dirname(descriptor_file)

  descriptor
}
