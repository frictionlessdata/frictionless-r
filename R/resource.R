#' Get or set a Data Resource
#'
#' @description
#' `resource()` gets a Data Resource from a Data Package by its name.
#' The returned value will be a list describing a Data Resource, with a new
#' attribute `data_location` to indicate how the data are attached.
#' If present, `path` will be updated to contain the full path(s).
#'
#' `resource<-` assigns a Data Resource to a Data Package by its name.
#' The assigned value is **not validated**, but changes made by `resource()`
#' will be reverted, i.e. the `data_location` attribute will be removed and
#' `path` will be shortened to its original value (without the prepended
#' `package$directory`).
#'
#' Note that these functions are **designed for internal use in other packages**.
#' For public manipulation of resources, use [add_resource()] and
#' [remove_resource()].
#'
#' @inheritParams read_resource
#' @returns List describing a Data Resource.
#' @family accessor functions
#' @export
#' @examples
#' # Load the example Data Package
#' package <- example_package()
#'
#' # Get the resource "deployments"
#' resource <- resource(package, "deployments")
#' str(resource)
#'
#' # Set the resource "deployments"
#' resource(package, "deployments") <- resource
resource <- function(package, resource_name) {
  # Check package
  check_package(package)

  # Check resource exists
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

  # Check that path or data are set
  # https://specs.frictionlessdata.io/data-resource/#data-location
  if (is.null(resource$path) && is.null(resource$data)) {
    cli::cli_abort(
      "Resource {.val {resource_name}} must have a {.field path} or
      {.field data} property.",
      class = "frictionless_error_resource_without_path_data"
    )
  }

  # Check that either path or data is set, not both
  if (all(c("path", "data") %in% names(resource))) {
    cli::cli_abort(
      "Resource {.val {resource_name}} must have a {.field path} or
       {.field data} property, not both.",
      class = "frictionless_error_resource_both_path_data"
    )
  }

  # Add data_location attribute (based on path, then df, then data)
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
        resource$path, ~ check_path(.x, attr(package, "directory"), safe = TRUE)
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

#' @rdname resource
#' @param value Data Resource to assign.
#' @return `package` with assigned resource.
#' @export
"resource<-" <- function(package, resource_name, value) {
  resource <- value

  # Check package
  check_package(package)

  # Check resource exists
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
  index <- which(resource_names(package) == resource_name)

  # Return early if resource is NULL
  if (is.null(resource)) {
    package$resources[[index]] <- resource
    return(package)
  }

  # Revert changes made by resource()
  # Remove directory from path, i.e. reverse check_path()
  directory <- attr(package, "directory")
  data_location <- attr(resource, "data_location") %||% "undefined"
  path_attr <- attr(resource, "path") %||% "undefined"
  out_paths <- vector()
  if (data_location == "path") {
    for (path in resource$path) {
      stripped_path <- gsub(paste0(directory, "/"), "", path)
      out_paths <- append(out_paths, stripped_path)
    }
    if (length(out_paths) > 1 && path_attr != "added") {
      # For existing resources with multiple paths, use list
      out_paths <- as.list(out_paths)
    }
    resource$path <- out_paths
  }
  # Remove data_location attribute
  attr(resource, "data_location") <- NULL

  # Assign resource
  package$resources[[index]] <- resource
  return(package)
}
