#' Add a Data Resource
#'
#' Adds a Tabular [Data
#' Resource](https://specs.frictionlessdata.io/data-resource/) to a Data
#' Package.
#' The resource will be a [Tabular Data
#' Resource](https://specs.frictionlessdata.io/tabular-data-resource/).
#' The resource name can only contain lowercase alphanumeric characters plus
#' `.`, `-` and `_`.
#'
#' @inheritParams read_resource
#' @param data Data to attach, either a data frame or path(s) to CSV file(s).
#'   - data frame: will be attached to the resource as `data` and written to a
#'     CSV file when using [write_package()].
#'   - path(s) to CSV file(s): will be added to the resource as `path`.
#'     The (last in case of multiple) CSV file will be read with
#'     [readr::read_csv()] using default parameters (comma-separated, headers
#'     present, etc.) only for the purpose of creating or comparing with a
#'     `schema`.
#' @param schema List object describing a Table Schema for the `data`.
#'   If not provided, one will be created using [create_schema()].
#' @return Provided `package` with one additional resource.
#' @family edit functions
#' @export
#' @examples
#' # Load the example Data Package
#' package <- example_package
#'
#' # List the resource names
#' package$resource_names
#'
#' # Create a data frame
#' df <- data.frame(
#'   multimedia_id = c(
#'     "aed5fa71-3ed4-4284-a6ba-3550d1a4de8d",
#'     "da81a501-8236-4cbd-aa95-4bc4b10a05df"
#'   ),
#'   x = c(718, 748),
#'   y = c(860, 900)
#' )
#'
#' # Add a new resource "positions" to the Data Package, from the data frame
#' package <- add_resource(package, "positions", data = df)
#'
#' # Add a new resource "deployments2" to the Data Package, from a CSV file path
#' path <- system.file("extdata", "deployments.csv", package = "frictionless")
#' package <- add_resource(package, "deployments2", data = path)
#'
#' # List the resource names ("positions" and "deployments2" added)
#' package$resource_names
add_resource <- function(package, resource_name, data, schema = NULL) {
  # Check package
  check_package(package)

  # Check resource name
  assertthat::assert_that(
    grepl(resource_name, pattern = "^[a-z0-9\\._-]+$"),
    msg = glue::glue(
      "`resource_name` `{resource_name}` must only contain lowercase",
      "alphanumeric characters plus `.`, `-` and `_`.",
      .sep = " "
    )
  )

  # Check resource is absent
  assertthat::assert_that(
    !resource_name %in% package$resource_names,
    msg = glue::glue(
      "`package` already contains a resource named `{resource_name}`."
    )
  )

  # Check data (df or path)
  assertthat::assert_that(
    is.data.frame(data) | is.character(data),
    msg = "`data` must be a data frame or path(s) to CSV file(s)."
  )
  if (is.data.frame(data)) {
    df <- data
  } else {
    paths <- purrr::map_chr(
      data, ~ check_path(.x, directory = NULL, unsafe = TRUE)
    )
    last_path <- paths[length(paths)]
    df <- readr::read_csv(last_path, progress = FALSE, show_col_types = FALSE)
  }

  # Create schema
  if (is.null(schema)) {
    schema <- create_schema(df)
  }

  # Check schema (also checks df)
  check_schema(schema, df)

  # Create resource, with properties in specific order
  if (is.data.frame(data)) {
    resource <- list(
      name = resource_name,
      data = df,
      profile = "tabular-data-resource", # Necessary for read_resource()
      schema = schema
      # other properties are set by write_resource()
    )
  } else {
    resource <- list(
      name = resource_name,
      path = paths,
      profile = "tabular-data-resource", # Necessary for read_resource()
      format = "csv",
      mediatype = "text/csv",
      # encoding: not set, not necessarily "utf-8"
      schema = schema
    )
  }

  # Add resource (needs to be wrapped in its own list)
  package$resources <- append(package$resources, list(resource))

  # Add resource_name
  package$resource_names <- append(package$resource_names, resource_name)

  package
}
