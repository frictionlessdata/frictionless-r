#' Check Data Package object
#'
#' Check if an object is a list object describing a Data Package, i.e. a list
#' with the required properties.
#'
#' @param package List object describing a Data Package.
#' @return `TRUE` or error.
#' @family check functions
#' @noRd
check_package <- function(package) {
  msg_invalid <- glue::glue(
    "`package` must be a list object created with `read_package()` or",
    "`create_package()`.",
    .sep = " "
  )
  # Check package is list with required properties
  assertthat::assert_that(
    is.list(package) &
      all(c("resources", "resource_names", "directory") %in% names(package)),
    msg = msg_invalid
  )

  # Check package properties have correct class
  assertthat::assert_that(
    is.list(package$resources) &
      is.character(package$resource_names) &
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

  # Check resource_names are in sync with resources name
  unknown_names <- setdiff(
    package$resource_names, purrr::map_chr(package$resources, ~ .x$name)
  )
  unknown_names_collapse <- paste(unknown_names, collapse = "`, `")
  assertthat::assert_that(
    length(unknown_names) == 0,
    msg = glue::glue(
      "Can't find resource(s) with name(s) `{unknown_names_collapse}`.",
      "\u2139 Is `package$resource_names` out of sync with `name` of resources?",
      .sep = "\n"
    )
  )
}
