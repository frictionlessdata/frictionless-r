#' Get or overwrite a Data Resource
#'
#' @description
#' Gets or overwrites a Data Resource from/in a Data Package.
#' These functions are **designed for internal use in other packages**.
#' For public manipulation of resources, use [add_resource()] and
#' [remove_resource()].
#'
#' `resource()` gets a Data Resource from a Data Package by its name.
#' The returned value will be a list describing a Data Resource, with a new
#' attribute `data_location` to indicate how the data are attached.
#' If present, `path` will be updated to the full path(s).
#'
#' `resource<-` overwrites a Data Resource in Data Package with a new value.
#' The assigned value will typically be a resource obtained with `resource()`
#' and then manipulated.
#' The assignment function will therefore revert internal changes made by
#' `resource()` (i.e. removing the attribute `data_location` and updating paths
#' to the original values).
#' The assigned value is otherwise **not validated**, so use with care.
#'
#' @details
#' See `vignette("data-resource")` to learn more about Data Resource.
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
#' # Update the resource
#' resource$description <- "Table with deployments."
#'
#' # Overwrite the resource
#' resource(package, "deployments") <- resource
#'
#' # Updating a resource property can also be done in one step
#' resource(package, "deployments")$description <- "Table with deployments."
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
#' @param value Value to assign.
#' @return `package` with overwritten resource.
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
