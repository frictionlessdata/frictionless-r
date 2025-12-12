#' Get a Data Resource
#'
#' Returns a Data Resource from a Data Package, i.e. the content of one of the
#' described `resources`.
#'
#' @inheritParams read_resource
#' @return List describing a Data Resource, with new attribute `data_location`
#'   to indicate how the data are attached.
#'   If present, `path` will be updated to contain the full path(s).
#' @family accessor functions
#' @noRd
resource <- function(package, resource_name) {
  # Check package
  check_package(package)

  # Check resource
  resource_names <- resource_names(package)
  if (!resource_name %in% resource_names) {
    cli::cli_abort(
      c(
        "Can't find resource {.val {resource_name}} in {.arg package}.",
        "i" = "Available resource{?s}: {.val {resource_names}}."
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

  # Assign data_location attribute (based on path, then df, then data)
  if (length(resource$path) != 0) {
    if (all(is_url(resource$path))) {
      data_location <- "url"
    } else {
      data_location <- "path"
    }
    # Expand paths to full paths, check if file exists and check path safety,
    # unless those paths were willingly added by user in add_resource()
    if (attr(resource, "path") %||% "" != "added") {
      resource$path <- purrr::map_chr(
        resource$path, ~ check_path(.x, package$directory, safe = TRUE)
      )
    }
  } else if (is.data.frame(resource$data)) {
    data_location <- "df"
  } else if (!is.null(resource$data)) {
    data_location <- "data"
  }
  attr(resource, "data_location") <- data_location

  return(resource)
}
