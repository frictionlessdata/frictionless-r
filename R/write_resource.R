#' Write a Data Resource to disk
#'
#' Helper function used by [write_package()] to write Data Resources to disk.
#'
#' @inheritParams read_resource
#' @inheritParams write_package
#' @returns Updated list describing a Data Resource, ready to be included in a
#'   `datapackage.json`.
#' @family write functions
#' @noRd
write_resource <- function(package, resource_name, directory = ".",
                           compress = FALSE) {
  # Get resource, includes check_package()
  resource <- resource(package, resource_name)

  # Resource contains new data
  data_location <- attr(resource, "data_location")
  if (data_location == "df") {
    if (compress) {
      file_name <- paste(resource_name, "csv", "gz", sep = ".")
    } else {
      file_name <- paste(resource_name, "csv", sep = ".")
    }
    readr::write_csv(resource$data, fs::path(directory, file_name), na = "")

    # Set properties
    resource <- append(resource, c(path = file_name), after = 1) # After "name"
    resource$format <- "csv"
    resource$mediatype <- "text/csv"
    resource$encoding <- "utf-8" # Enforced by readr::write_csv()
    resource$dialect <- NULL
    resource$data <- NULL

  # Resource originally had data property
  } else if (data_location == "data") {
    # Do nothing

  # Resource has local paths (optionally mixed with URLs)
  } else if (data_location == "path") {
    out_paths <- vector()
    for (path in resource$path) {
      file_name <- fs::path_file(path)
      destination <- fs::path(directory, file_name)

      # Download file to destination
      if (is_url(path)) {
        if (!fs::file_exists(destination)) {
          cli::cli_inform(
            "Downloading file from {.path {path}}.",
            class = "frictionless_message_file_downloading"
          )
          utils::download.file(path, destination, quiet = TRUE)
        }

      # Copy file to destination, but don't overwrite if it was read from there
      } else {
        if (fs::path_norm(path) != fs::path_norm(destination) &&
            fs::file_exists(destination)) {
          fs::file_delete(destination)
        }
        fs::file_copy(path, destination, overwrite = FALSE)
      }
      out_paths <- append(out_paths, file_name)
    }
    if (length(out_paths) > 1) {
      # String for single file, pretty array for multiple files
      out_paths <- as.list(out_paths)
    }
    resource$path <- out_paths

  # Resource has URL paths (only)
  } else if (data_location == "url") {
    # Don't touch file, leave URL path as is
  }

  # Remove attributes
  attr(resource, "data_location") <- NULL

  return(resource)
}
