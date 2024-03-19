#' Write a Data Package to disk
#'
#' Writes a Data Package and its related Data Resources to disk as a
#' `datapackage.json` and CSV files.
#' Already existing CSV files of the same name will not be overwritten.
#' The function can also be used to download a Data Package in its entirety.
#' The Data Resources are handled as follows:
#' - Resource `path` has at least one local path (e.g. `deployments.csv`):
#'   CSV files are copied or downloaded to `directory` and `path` points to new
#'   location of file(s).
#' - Resource `path` has only URL(s): resource stays as is.
#' - Resource has inline `data` originally: resource stays as is.
#' - Resource has inline `data` as result of adding data with `add_resource()`:
#'   data are written to a CSV file using [readr::write_csv()], `path` points to
#'   location of file, `data` property is removed.
#'   Use `compress = TRUE` to gzip those CSV files.
#' @param package List describing a Data Package, created with [read_package()]
#'   or [create_package()].
#' @param directory Path to local directory to write files to.
#' @param compress If `TRUE`, data of added resources will be gzip compressed
#'   before being written to disk (e.g. `deployments.csv.gz`).
#' @return `package` as written to file (invisibly).
#' @family write functions
#' @export
#' @examples
#' # Load the example Data Package from disk
#' package <- read_package(
#'   system.file("extdata", "datapackage.json", package = "frictionless")
#' )
#'
#' # List resources
#' resources(package)
#'
#' # Write the (unchanged) Data Package to disk
#' write_package(package, directory = "my_directory")
#'
#' # Check files
#' list.files("my_directory")
#'
#' # No files written for the "observations" resource, since those are all URLs.
#' # No files written for the "media" resource, since it has inline data.
#'
#' # Clean up (don't do this if you want to keep your files)
#' unlink("my_directory", recursive = TRUE)
write_package <- function(package, directory = ".", compress = FALSE) {
  # Check package
  check_package(package)

  # Check resources
  if (length(package$resources) == 0) {
    cli::cli_abort(
      c(
        "{.arg package} must have resources.",
        "i" = "Use {.fun add_resource} to add resources."
      ),
      class = "frictionless_error_package_without_resources"
    )
  }

  # Create directory if it doesn't exists yet
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }

  # Write resources to disk + update paths
  out_resources <- list()
  for (i in seq_along(package$resources)) {
    resource_name <- package$resources[[i]]$name
    out_resource <- write_resource(package, resource_name, directory, compress)
    out_resources[[i]] <- out_resource
  }

  # Update package
  package$resources <- out_resources
  return_package <- package # Needs directory to remain valid

  # Write datapackage.json
  package$directory <- NULL
  package_json <- jsonlite::toJSON(package, pretty = TRUE, auto_unbox = TRUE)
  write(package_json, file.path(directory, "datapackage.json"))

  # Return (updated) package invisibly
  invisible(return_package)
}
