#' Get a Data Resource
#'
#' Returns a [Data Resource](https://specs.frictionlessdata.io/data-resource/)
#' from a Data Package, i.e. the content of one of the described `resources`.
#'
#' @inheritParams read_resource
#' @return List object describing a Data Resource, with new property `read_from`
#'   to indicate how data should be read. If present, `path` will be updated to
#'   contain the full path(s).
#' @noRd
#' @examples
#' # Load the example Data Package
#' package <- example_package
#'
#' # Get the Data Resource "observations"
#' resource <- get_resource(package, "observations")
#' str(resource)
get_resource <- function(package, resource_name) {
  # Check package
  check_package(package)

  # Check resource
  resource_names_collapse <- paste(package$resource_names, collapse = ", ")
  assertthat::assert_that(
    resource_name %in% package$resource_names,
    msg = glue::glue(
      "Can't find resource `{resource_name}` in `{resource_names_collapse}`."
    )
  )

  # Get resource
  resource <- purrr::keep(package$resources, ~ .x$name == resource_name)[[1]]

  # Check path(s) to file(s)
  # https://specs.frictionlessdata.io/data-resource/#data-location
  assertthat::assert_that(
    !is.null(resource$path) | !is.null(resource$data),
    msg = glue::glue(
      "Resource `{resource_name}` must have property `path` or `data`."
    )
  )

  # Assign read_from property (based on path, then df, then data)
  if (length(resource$path) != 0) {
    resource$read_from <- "path"
    # Update paths to full paths
    resource$path <- purrr::map_chr(
      resource$path, ~ check_path(.x, package$directory, unsafe = FALSE)
    )
  } else if (is.data.frame(resource$data)) {
    resource$read_from <- "df"
  } else if (!is.null(resource$data)) {
    resource$read_from <- "data"
  }

  resource
}
