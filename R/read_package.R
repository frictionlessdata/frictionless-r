#' Read a Data Package descriptor file (`datapackage.json`)
#'
#' Reads information from a `datapackage.json` file, i.e. the
#' [descriptor](https://specs.frictionlessdata.io/data-package/#descriptor) that
#' describes the Data Package metadata and its resources.
#'
#' @param file Path or URL to a `datapackage.json` file.
#' @return List object describing a Data Package, i.e the content of the
#' descriptor file and two new properties:
#'   - `resource_names`: vector with resource names.
#'   - `directory`: path to Data Package directory, used as root path to read
#'     resources with [read_resource()].
#' @export
#' @examples
#' # Read a datapackage.json file
#' package <- read_package(
#'   system.file("extdata", "datapackage.json", package = "frictionless")
#' )
#'
#' # Access the package properties
#' package$name
#' package$resource_names
read_package <- function(file = "datapackage.json") {
  # Read file
  file <- check_path(file)
  descriptor <- jsonlite::fromJSON(file, simplifyDataFrame = FALSE)

  # Check resources
  # https://specs.frictionlessdata.io/data-package/#metadata
  assertthat::assert_that(
    length(descriptor$resources) != 0 & # Null or empty list
    purrr::every(descriptor$resources, ~ !is.null(.x$name)),
    msg = glue::glue(
      "Descriptor `{file}` must have property `resources` containing at least",
      "one resource. All resources must have a `name`.", .sep = " "
    )
  )

  # Add datapackage class
  class(descriptor) <- c("datapackage", class(descriptor))

  # Add resource_names
  descriptor$resource_names <- purrr::map_chr(descriptor$resources, "name")

  # Add directory
  descriptor$directory <- dirname(file) # Also works for URLs

  # Inform user regarding rights/citations
  msg <- glue::glue(
    "Please make sure you have the right to access data from this Data Package",
    "for your intended use.\nFollow applicable norms or requirements to credit",
    "the dataset and its authors.", .sep = " "
  )
  if (!is.null(descriptor$id)) {
    if (startsWith(descriptor$id, "http")) {
      msg <- glue::glue(
        "{msg}", "For more information, see {descriptor$id}", .sep = "\n"
      )
    }
  }
  message(msg)

  descriptor
}
