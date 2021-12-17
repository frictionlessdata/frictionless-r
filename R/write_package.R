#' Write a Data Package to disk
#'
#' Writes a Data Package and its related Data Resources to disk as a
#' `datapackage.json` and csv files. Originally included resources are left
#' untouched, those added with [add_resource()] are written to file using
#' [readr::write_csv()].
#'
#' @param package List object describing a Data Package, created with
#'   [read_package()] or [create_package()].
#' @param directory Path to local directory to write files to.
#' @return Provided `package` (invisibly).
#' @family write functions
#' @export
#' @examples
#' # Load the example Data Package from disk
#' package <- read_package(
#'   system.file("extdata", "datapackage.json", package = "frictionless")
#' )
#'
#' # List the resource names
#' package$resource_names
#'
#' # Write the (unchanged) Data Package to disk
#' write_package(package, directory = "my_package")
#'
#' # Check files
#' list.files("my_package")
#'
#' # Clean up (don't do this if you want to keep your files)
#' unlink("my_package", recursive = TRUE)
write_package <- function(package, directory = ".") {
  orig_package <- package
  # Check package
  check_package(package)

  # Check resources
  assertthat::assert_that(
    length(package$resources) != 0, # Null or empty list
    msg = glue::glue(
      "`package` must have resources. Use `add_resource()` to add resources.",
      .sep = " "
    )
  )

  # Make directory if it doesn't exists yet
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }

  # Write resources to disk + update paths
  out_resources <- list()
  for (i in seq_along(package$resources)) {
    resource_name <- package$resources[[i]]$name
    out_resource <- write_resource(package, resource_name, directory)
    out_resources[[i]] <- out_resource
  }

  # Update package
  package$resources <- out_resources
  package$directory <- NULL
  package$resource_names <- NULL

  # Write datapackage.json
  package_json <- jsonlite::toJSON(package, pretty = TRUE, auto_unbox = TRUE)
  write(package_json, file.path(directory, "datapackage.json"))

  # Return (updated) package invisibly
  invisible(orig_package)
}
