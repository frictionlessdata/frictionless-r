#' Read a Data Package descriptor file (`datapackage.json`)
#'
#' Reads information from a `datapackage.json` file, i.e. the [descriptor](
#' https://specs.frictionlessdata.io/data-package/#descriptor) file that
#' describes the Data Package metadata and its Data Resources.
#'
#' See `vignette("data-package")` to learn how this function implements the
#' Data Package standard.
#'
#' @param file Path or URL to a `datapackage.json` file.
#' @param attach Attach the resources' data to the package object rather than keeping the path. See
#' [data location](https://specs.frictionlessdata.io/data-resource/#data-location).
#' @return A Data Package object, see [create_package()].
#' @family read functions
#' @export
#' @examples
#' # Read a datapackage.json file
#' package <- read_package(
#'   system.file("extdata", "v1", "datapackage.json", package = "frictionless")
#' )
#'
#' package
#'
#' # Access the Data Package properties
#' package$name
#' package$created
read_package <- function(file = "datapackage.json", attach = FALSE) {
  # Read file
  if (!is.character(file)) {
    cli::cli_abort(
      "{.arg file} must be a path or URL to a {.file datapackage.json} file.",
      class = "frictionless_error_file_invalid"
    )
  }
  descriptor <- read_descriptor(file, safe = FALSE)

  # Warn if resources is absent
  if (length(descriptor$resources) == 0) {
    cli::cli_warn(
      c(
        "{.arg file} {.file {file}} should have a {.field resources} property
         containing at least one resource.",
        "i" = "Use {.fun add_resource} to add resources."
      ),
      class = "frictionless_warning_file_without_resources"
    )
  }

  # Add directory
  descriptor$directory <- dirname(file) # Also works for URLs

  # Create package
  package <- create_package(descriptor)

  # Attach path and url to package
  if (attach) {
    package$resources <- purrr::map(package$resources, ~ {
      resource <- get_resource(package, .x$name)
      if (resource$read_from %in% c("path", "url")) {
        .x$data <- read_from_path(package, .x$name, col_select = NULL)
        .x$path <- NULL
      }
      .x
    })
  }

  return(package)
}
