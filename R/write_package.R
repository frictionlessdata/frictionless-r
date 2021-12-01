#' Write a Data Package to disk
#'
#' Writes a Data Package and its related Data Resources to disk as a
#' `datapackage.json` and csv files. Originally included Data Resources are left
#' untouched, those added with _add_resource()_ are written to file using
#' [readr::write_csv()].
#'
#' @param package List object describing a Data Package, created with
#'   [read_package()] or [create_package()].
#' @param directory Path to local directory to write files to.
#' @return `package` invisibly.
#' @export
#' @examples
#' # TODO
write_package <- function(package, directory = ".") {
  # Check package
  check_package(package)

  # Make directory if it doesn't exists yet
  if (!dir.exists(directory)) {
    dir.create(directory)
  }

  # Check resources
  assertthat::assert_that(
    length(package$resources) != 0 & # Null or empty list
    purrr::every(package$resources, ~ !is.null(.x$name)),
    msg = glue::glue(
      "`package` must have resources (with a `name`). Use `add_resource()` to",
      "add resources.", .sep = " "
    )
  )

  # Write resources to disk + update paths
  out_resources <- list()
  for (i in seq_along(package$resources)) {
    resource_name <- package$resources[[i]]$name
    out_resource <- write_resource(resource_name, package, directory)
    out_resources[[i]] <- out_resource
  }

  # Update package
  package$resources <- out_resources
  package$directory <- NULL
  package$resource_names <- NULL

  # Write datapackage.json
  package_json <- jsonlite::toJSON(package, pretty = TRUE, auto_unbox = TRUE)
  write(package_json, paste(directory, "datapackage.json", sep = "/"))

  # Return (updated) package invisibly
  invisible(package)
}
