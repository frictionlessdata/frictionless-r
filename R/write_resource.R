#' Write a Data Resource to disk
#'
#' Helper function used by [write_package()] to write Data Resources to disk.
#'
#' @inheritParams read_resource
#' @param directory Path to local directory to write files to.
#' @return Updated list object describing a Data Resource, ready for including
#'   in a `datapackage.json`.
#' @noRd
write_resource <- function(resource_name, package, directory = ".") {
  resource <- get_resource(resource_name, package)

  # Resource contains new data
  if (resource$read_from == "df") {
    file_name <- paste(resource_name, "csv", sep = ".")
    readr::write_csv(resource$data, paste(directory, file_name, sep = "/"))
    resource$path <- file_name
    resource$data <- NULL
    resource$read_from <- NULL

  # Resource originally had data property
  } else if (resource$read_from == "data") {
    resource$read_from <- NULL

  # Resource has paths
  } else if (resource$read_from == "path") {
    out_paths <- c()
    for (path in resource$path) {
      if (startsWith(path, "http")) {
        # File at URL
        # Don't touch file, point path to URL. Note that the original path might
        # have been a local path, but datapackage.json was accessed via URL.
        out_paths <- append(out_paths, path)
      } else {
        # Local file
        # Copy file to directory, point path to file name (in that directory).
        file_name <- basename(path)
        file.copy(path, paste(directory, file_name, sep = "/"))
        out_paths <- append(out_paths, file_name)
      }
    }
    resource$path <- out_paths
    resource$read_from <- NULL
  }

  return(resource)
}
