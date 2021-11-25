#' Check package object
#'
#' Check if a package object is a list object of class `datapackage`.
#'
#' @param package Package object
#'
#' @return `TRUE` or error.
#'
#' @keywords internal
#'
#' @importFrom assertthat assert_that
#' @importFrom glue glue
check_package <- function(package) {
  assert_that(
    "datapackage" %in% class(package),
    msg = glue(
      "`package` must be a list object of class datapackage created with",
      "`read_package()` or `create_package()`.", .sep = " "
    )
  )
}
