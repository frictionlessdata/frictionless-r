#' Check Data Package object
#'
#' Check if an object is a list describing a Data Package, i.e. it has the
#' required properties `resources`, `resource_names` and `directory`.
#'
#' @param package List describing a Data Package.
#' @return `TRUE` or error.
#' @family check functions
#' @noRd
check_package <- function(package) {
  msg_invalid <- glue::glue(
    "`package` must be a list describing a Data Package,",
    "created with `read_package()` or `create_package()`.",
    .sep = " "
  )
  # Check package is list with required properties
  assertthat::assert_that(
    is.list(package) &
      all(c("resources", "directory") %in% names(package)),
    msg = msg_invalid
  )

  # Check package properties have correct class
  assertthat::assert_that(
    is.list(package$resources) &
      is.character(package$directory),
    msg = msg_invalid
  )

  # Check all resources (if any) have a name
  assertthat::assert_that(
    purrr::every(package$resources, ~ !is.null(.x$name)),
    msg = glue::glue(
      "All resources in `package` must have property `name`."
    )
  )
}
