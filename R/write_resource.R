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

  full_paths <- resource$full_path
  out_paths <- c()
  for (full_path in full_paths) {
    if (startsWith(full_path, "http")) {
      # File at URL
      # Don't touch file, point `path` to URL. Note that the original `path`
      # might have been a local path, but datapackage.json was accessed via URL.
      out_paths <- append(out_paths, full_path)
    } else {
      # Local file
      # Copy file to directory, point `path` to file name (in that directory).
      file_name <- basename(full_path)
      file.copy(full_path, paste(directory, file_name, sep = "/"))
      out_paths <- append(out_paths, file_name)
    }
  }

  resource$full_path <- NULL # Remove this added property
  resource$path <- out_paths

  return(resource)
}
