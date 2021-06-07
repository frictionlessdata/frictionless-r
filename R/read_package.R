#' Read a datapackage.json file
#'
#' Reads information from a `datapackage.json` file, i.e. the
#' [descriptor](https://specs.frictionlessdata.io/data-package/#descriptor) that
#' describes the Data Package metadata and its resources.
#'
#' @param file Path or URL to a `datapackage.json` file.
#'
#' @return List object containing the descriptor information and two new
#'   properties:
#'   - `resource_names`: vector with resource names.
#'   - `directory`: path to Data Package directory, used as root path to read
#'     resources with `read_resource()`.
#'
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom glue glue
#' @importFrom jsonlite fromJSON
#' @importFrom RCurl url.exists
#'
#' @examples
#' path <- system.file("extdata", "datapackage.json", package = "datapackage")
#' package <- read_package(path)
#' package$resource_names
read_package <- function(file = "datapackage.json") {
  # Read file
  assert_that(
    file.exists(file) | url.exists(file),
    msg = glue("Could not find file at '{file}'.")
  )
  descriptor <- fromJSON(file, simplifyDataFrame = FALSE)

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
  descriptor$directory <- dirname(file)

  descriptor
}
