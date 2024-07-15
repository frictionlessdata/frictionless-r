#' Get a Data Resource
#'
#' Returns a [Data Resource](https://specs.frictionlessdata.io/data-resource/)
#' from a Data Package, i.e. the content of one of the described `resources`.
#'
#' @inheritParams read_resource
#' @return List describing a Data Resource, with new property `read_from` to
#'   indicate how data should be read.
#'   If present, `path` will be updated to contain the full path(s).
#' @family edit functions
#' @noRd
#' @examples
#' # Load the example Data Package
#' package <- example_package()
#'
#' # Get the resource "observations"
#' resource <- frictionless:::get_resource(package, "observations")
#' str(resource)
get_resource <- function(package, resource_name) {
  # Check package
  check_package(package)

  # Check resource
  resource_names <- resources(package)
  if (!resource_name %in% resources(package)) {
    cli::cli_abort(
      c(
        "Can't find resource {.val {resource_name}} in {.arg package}.",
        "i" = "Available resource{?s}: {.val {resources(package)}}."
      ),
      class = "frictionless_error_resource_not_found"
    )
  }

  # Get resource
  resource <- purrr::keep(package$resources, ~ .x$name == resource_name)[[1]]

  # Check path(s) to file(s)
  # https://specs.frictionlessdata.io/data-resource/#data-location
  if (is.null(resource$path) && is.null(resource$data)) {
    cli::cli_abort(
      "Resource {.val {resource_name}} must have a {.field path} or
      {.field data} property.",
      class = "frictionless_error_resource_without_path_data"
    )
  }

  # Check that either data or path is set, not both
  if (all(c("data", "path") %in% names(resource))) {
    cli::cli_abort(
      "Resource {.val {resource_name}} must have a {.field path} or
       {.field data} property, not both.",
      class = "frictionless_error_resource_both_path_data"
    )
  }

  # Assign read_from property (based on path, then df, then data)
  if (length(resource$path) != 0) {
    if (all(is_url(resource$path))) {
      resource$read_from <- "url"
    } else {
      resource$read_from <- "path"
    }
    # Expand paths to full paths, check if file exists and check path safety,
    # unless those paths were willingly added by user in add_resource()
    if (attr(resource, "path") %||% "" != "added") {
      resource$path <- purrr::map_chr(
        resource$path, ~ check_path(.x, package$directory, safe = TRUE)
      )
    }
  } else if (is.data.frame(resource$data)) {
    resource$read_from <- "df"
  } else if (!is.null(resource$data)) {
    resource$read_from <- "data"
  }

  return(resource)
}
