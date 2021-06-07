#' Read a datapackage.json file
#'
#' Reads information from a `datapackage.json` file, i.e. the
#' [descriptor](https://specs.frictionlessdata.io/data-package/#descriptor) that
#' describes the Data Package metadata and its resources.
#'
#' @param descriptor_file Path to a `datapackage.json` file.
#'
#' @return List object containing the descriptor information and two new
#'   properties:
#'   - `resource_names`: vector with resource names.
#'   - `directory`: directory path of descriptor file, used as base path to read
#'     resources with `read_resource()`.
#'
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom glue glue
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' path <- system.file("extdata", "datapackage.json", package = "datapackage")
#' package <- read_package(path)
#' package$resource_names
read_package <- function(descriptor_file = "datapackage.json") {
  descriptor <- fromJSON(descriptor_file, simplifyDataFrame = FALSE)

  # Check for resources
  # https://specs.frictionlessdata.io/data-package/#metadata
  assert_that(
    !is.null(descriptor$resources),
    msg = glue(
      "'{descriptor_file}' does not have the required property 'resources'."
    )
  )

  # Add resource_names
  descriptor$resource_names <- map_chr(descriptor$resources, "name")

  # Add directory
  descriptor$directory <- dirname(descriptor_file)

  descriptor
}
