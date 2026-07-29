#' Write a Data Package to disk
#'
#' Writes a Data Package and its related Data Resources to disk as a
#' `datapackage.json`.
#' Depending how the data are attached to the Data Resource, the function will
#' also write or copy these to CSV files.
#'
#' @section Writing data to CSV files:
#'
#' For Data Resources added or replaced with `add_resource()`, with data
#' attached as:
#'
#' - A data frame: data are written to a CSV file in `directory` using
#'   `readr::write_csv()` and `path` is added.
#'   The CSV file will have the same name as the resource and can be compressed
#'   with `compress = TRUE`.
#' - One or more paths to local CSV file(s): CSV file(s) are copied to
#'   `directory` and `path` is updated to the new location.
#' - URL(s) to CSV file(s): no CSV files are written.
#'
#' For existing Data Resources that were not manipulated, with data attached as:
#'
#' - One or more paths to local CSV file(s): CSV file(s) are copied to
#'   `directory` and `path` is updated to the new location.
#'   Existing CSV files of the same name in `directory` are _not overwritten_.
#'   This allows you to read and write a `datapackage.json` to the same
#'   directory, without altering the files of resources you did not manipulate.
#' - URL(s) to CSV file(s): no CSV files are written.
#' - Inline data: no CSV files are written.
#'
#' @inheritParams read_resource
#' @param directory Path to local directory to write files to.
#' @param compress If `TRUE`, data of added resources will be gzip compressed
#'   before being written to disk (e.g. `deployments.csv.gz`).
#' @returns `package` invisibly, as written to file.
#' @family write functions
#' @export
#' @examples
#' # Load the example Data Package from disk
#' package <- read_package(
#'   system.file("extdata", "v1", "datapackage.json", package = "frictionless")
#' )
#'
#' package
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
write_package <- function(package, directory, compress = FALSE) {
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
  attr(package, "directory") <- NULL
  package_json <- jsonlite::toJSON(
    package,
    pretty = TRUE,
    null = "null",
    na = "null",
    auto_unbox = TRUE
  )
  write(package_json, file.path(directory, "datapackage.json"))

  # Return (updated) package invisibly
  invisible(return_package)
}
