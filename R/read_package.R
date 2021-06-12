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
#' @importFrom httr http_error
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' path <- system.file("extdata", "datapackage.json", package = "datapackage")
#' package <- read_package(path)
#' package$resource_names
read_package <- function(file = "datapackage.json") {
  # Read file
  if (startsWith(file, "http")) {
    assert_that(
      !http_error(file),
      msg = glue("Can't find file at `{file}`.")
    )
  } else {
    assert_that(
      file.exists(file),
      msg = glue("Can't find file at `{file}`.")
    )
  }
  descriptor <- fromJSON(file, simplifyDataFrame = FALSE)

  # Check for resources
  # https://specs.frictionlessdata.io/data-package/#metadata
  assert_that(
    !is.null(descriptor$resources[[1]]$name),
    msg = glue(
      "Descriptor `{file}` must have property `resources` containing at least ",
      "one resource with a `name`."
    )
  )

  # Add resource_names
  descriptor$resource_names <- map_chr(descriptor$resources, "name")

  # Add directory
  descriptor$directory <- dirname(file) # Also works for URLs

  descriptor
}
