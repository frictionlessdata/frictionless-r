#' Write a Data Resource to disk
#'
#' Helper function used by [write_package()] to write Data Resources to disk.
#'
#' @inheritParams read_resource
#' @param directory Path to local directory to write files to.
#' @return Updated list object describing a Data Resource, ready to be included
#'   in a `datapackage.json`.
#' @family write functions
#' @noRd
write_resource <- function(package, resource_name, directory = ".") {
  resource <- get_resource(package, resource_name)

  # Resource contains new data
  if (resource$read_from == "df") {
    file_name <- paste(resource_name, "csv", sep = ".")
    readr::write_csv(resource$data, file.path(directory, file_name))

    # Save schema and reassign all resource properties (in correct order)
    # This also removes $data and $read_from
    schema <- resource$schema
    resource <- list(
      name = resource_name,
      path = file_name,
      profile = "tabular-data-resource",
      # title: not set
      # description: not set
      format = "csv",
      mediatype = "text/csv",
      encoding = "utf-8", # Enforced by readr::write_csv()
      # dialect: not set, default
      # bytes: not set
      # hash: not set
      # sources: not set
      # licenses: not set
      schema = schema
    )
  # Resource originally had data property
  } else if (resource$read_from == "data") {
    resource$read_from <- NULL

  # Resource has local paths
  } else if (resource$read_from == "path") {
    # Download or copy file to directory, point path to file name (in that dir)
    # Note that existing files will not be overwritten
    out_paths <- vector()
    for (path in resource$path) {
      file_name <- basename(path)
      destination <- file.path(directory, file_name)
      if (startsWith(path, "http")) {
        if (!file.exists(destination)) {
          download.file(path, destination)
        }
      } else {
        file.copy(path, destination, overwrite = FALSE)
      }
      out_paths <- append(out_paths, file_name)
    }
    resource$read_from <- NULL
    resource$path <- out_paths

  # Resource has URL paths (only)
  } else if (resource$read_from == "url") {
    # Don't touch file, leave URL path as is
    resource$read_from <- NULL
  }

  return(resource)
}
