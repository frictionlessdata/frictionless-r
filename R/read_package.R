#' Read a Data Package descriptor file (`datapackage.json`)
#'
#' Reads information from a `datapackage.json` file, i.e. the
#' [descriptor](https://specs.frictionlessdata.io/data-package/#descriptor) file
#' that describes the Data Package metadata and its Data Resources.
#'
#' @param file Path or URL to a `datapackage.json` file.
#' @return List describing a Data Package.
#'   The function will add a custom property `directory` with the directory the
#'   descriptor was read from.
#'   It is used as a base path to access resources.
#' @family read functions
#' @export
#' @examples
#' # Read a datapackage.json file
#' package <- read_package(
#'   system.file("extdata", "datapackage.json", package = "frictionless")
#' )
#'
#' # Access the Data Package properties
#' package$name
#' package$created
#'
#' # List resources
#' resources(package)
read_package <- function(file = "datapackage.json") {
  # Read file
  if (!is.character(file)) {
    cli::cli_abort(
      "{.arg file} must be a path or URL to a {.file datapackage.json} file.",
      class = "frictionless_error_file_invalid"
    )
  }
  descriptor <- read_descriptor(file, safe = FALSE)

  # Check resources are present
  # Checking that they have a name is done when accessing, see check_package()
  # https://specs.frictionlessdata.io/data-package/#metadata
  if (length(descriptor$resources) == 0) {
    cli::cli_abort(
      "{.arg file} {.file {file}} must have a {.field resources} property
       containing at least one resource.",
      class = "frictionless_error_descriptor_without_resources"
    )
  }

  # Add directory
  descriptor$directory <- dirname(file) # Also works for URLs

  # Inform user regarding rights/citations
  msg <- glue::glue(
    "Please make sure you have the right to access data from this Data Package",
    "for your intended use.\nFollow applicable norms or requirements to credit",
    "the dataset and its authors.",
    .sep = " "
  )
  if (!is.null(descriptor$id)) {
    if (startsWith(descriptor$id, "http")) {
      msg <- glue::glue(
        "{msg}", "For more information, see {descriptor$id}",
        .sep = "\n"
      )
    }
  }
  message(msg)

  descriptor
}
