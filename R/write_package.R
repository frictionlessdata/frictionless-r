#' Write a Data Package to disk
#'
#' Writes a Data Package and its related Data Resources to disk as a
#' `datapackage.json` and CSV files.
#' The Data Resources are handled as follows:
#' - For a new resource added with [add_resource()], attached `data` will be
#'   written to a CSV file in `directory` using [readr::write_csv()]).
#'   The file will have the same name as the resource, use `compress = TRUE` to
#'   gzip it.
#' - For an existing resource with one or more local files in `path`, files will
#'   be copied to `directory` and `path` will updated to the new location of the
#'   file(s).
#'   If the `directory` is the same as the one from which the package was read,
#'   the resource remains as is and no CSV files will be overwritten.
#' - For an existing resource with only URLs in `path`, the resource remains as
#'   is and no CSV files will be written.
#' - For an existing resource with inline `data`, the resource remains as is and
#'   no CSV files will be written.
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
