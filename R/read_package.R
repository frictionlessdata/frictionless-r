#' Read a Data Package descriptor file (`datapackage.json`)
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
#'
#' @examples
#' # Read datapackage.json file
#' package <- read_package(system.file("extdata", "datapackage.json", package = "datapackage"))
#'
#' # Access package properties
#' package$name
#' package$resource_names
read_package <- function(file = "datapackage.json") {
  # Read file
  file <- check_path(file)
  descriptor <- fromJSON(file, simplifyDataFrame = FALSE)

  # Check for resources
  # https://specs.frictionlessdata.io/data-package/#metadata
  assert_that(
    !is.null(descriptor$resources[[1]]$name),
    msg = glue(
      "Descriptor `{file}` must have property `resources` containing at least",
      "one resource with a `name`.", .sep = " "
    )
  )

  # Add class
  class(descriptor) <- c("datapackage", class(descriptor))

  # Add resource_names
  descriptor$resource_names <- map_chr(descriptor$resources, "name")

  # Add directory
  descriptor$directory <- dirname(file) # Also works for URLs

  # Inform user regarding rights/citations
  msg <- glue(
    "Please make sure you have the right to access data from this Data Package",
    "for your proposed use.\nFollow applicable norms or requirements to credit",
    "the dataset and its authors.", .sep = " "
  )
  if (!is.null(descriptor$id)) {
    if (startsWith(descriptor$id, "http")) {
      msg <- glue(
        "{msg}", "For more information, see {descriptor$id}", .sep = "\n"
      )
    }
  }
  message(msg)

  descriptor
}
