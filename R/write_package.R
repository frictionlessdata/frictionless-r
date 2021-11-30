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
    !is.null(package$resources[[1]]$name),
    msg = glue::glue(
      "`package` must have property `resources` containing at least",
      "one resource with a `name`.", .sep = " "
    )
  )

  invisible(package)
}
