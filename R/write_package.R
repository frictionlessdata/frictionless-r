#' Write a Data Package to disk
#'
#' Writes a Data Package and its related Data Resources to disk as a
#' `datapackage.json` and csv files (using [readr::write_csv()]).
#'
#' @param package List object describing a Data Package, created with
#'   [read_package()] or [create_package()].
#' @param directory Path to local directory to write files to.
#' @return package` invisibly.
#' @export
#' @examples
#' # TODO
write_package <- function(package, directory = ".") {
  # Check package
  check_package(package)

  # Check for resources
  assertthat::assert_that(
    length(package$resources) != 0 & # Null or empty list
    purrr::every(package$resources, ~ !is.null(.x$name)),
    msg = glue::glue(
      "`package` must have resources (with a `name`). Use `add_resource()` to",
      "add resources.", .sep = " "
    )
  )

  invisible(package)
}
