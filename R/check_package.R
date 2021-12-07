#' Check Data Package object
#'
#' Check if an object is a list object describing a Data Package, i.e. a list
#' object of class `datapackage`, with the required properties.
#'
#' @param package List object describing a Data Package.
#' @return `TRUE` or error.
#' @noRd
check_package <- function(package) {
  # Check class, properties and types
  assertthat::assert_that(
    all(c("datapackage", "list") %in% class(package)) &
      all(c("resources", "resource_names", "directory") %in% names(package)) &
      is.list(package$resources) &
      is.character(package$resource_names) &
      is.character(package$directory),
    msg = glue::glue(
      "`package` must be a list object of class `datapackage` created with",
      "`read_package()` or `create_package()`.", .sep = " "
    )
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
  unknown_names_collapse <- paste(unknown_names, collapse = ", ")
  assertthat::assert_that(
    length(unknown_names) == 0,
    msg = glue::glue(
      "Can't find resource with name `{unknown_names}`.",
      "* Is `package$resource_names` out of sync with names of resources?",
      .sep = "\n"
    )
  )
}