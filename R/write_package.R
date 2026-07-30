#' Write a Data Package to disk
#'
#' Writes a Data Package and its related Data Resources to disk as a
#' `datapackage.json` and CSV files.
#'
#' @section Writing data to CSV files:
#'
#' `write_package()` will write data to CSV files depending on how these are
#' attached to the Data Resource:
#'
#' #### Data frame
#'
#' ```R
#' add_resource(package, "media", data = df)
#' ```
#'
#' Data are **written** to a CSV file in `directory` using [readr::write_csv()].
#' The CSV file will have the same name as the resource, overwriting any
#' existing file with the same name.
#' Use `compress = TRUE` to gzip the CSV file.
#'
#' #### One or more paths to local CSV files in a _different directory_
#'
#' ```R
#' add_resource(package, "media", data = "other-directory/media.csv")
#' ```
#'
#' CSV files are **copied** to `directory`, overwriting existing files with the
#' same name.
#'
#' #### One or more paths to local CSV files in the _same directory_
#'
#' ```R
#' add_resource(package, "media", data = "directory/media.csv")
#' ```
#'
#' CSV files are left **as is** (no overwrite).
#' This allows you to read and write a `datapackage.json` to the same directory,
#' without altering the CSV files of resources you did not manipulate.
#'
#' #### One or more URLs to CSV files
#'
#' ```
#' add_resource(package, "media", data = "https://example.org/media.csv")
#' ```
#'
#' Files are **not downloaded**.
#'
#' #### Mix of URLs and paths to CSV files
#'
#' ```R
#' add_resource(
#'   package, "media",
#'   data = c("https://example.org/media.csv", "media.csv")
#' )
#' ```
#'
#' Remote CSV files are **downloaded** to `directory`, overwriting existing
#' files with the same name.
#' Local CSV files are handled as described above.
#'
#' #### Inline data
#'
#' **No files are written**.
#'
#' In all above cases `path` is added or updated to the new file location(s)
#' when appropriate.
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
