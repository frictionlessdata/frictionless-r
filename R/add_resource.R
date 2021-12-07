#' Add a Data Resource
#'
#' Adds a Tabular
#' [Data Resource](https://specs.frictionlessdata.io/data-resource/)
#' to a Data Package. The resource will be a
#' [Tabular Data Resource](https://specs.frictionlessdata.io/tabular-data-resource/).
#' The resource name can only contain lowercase alphanumeric characters plus
#' `.`, `-` and `_`.
#'
#' @inheritParams read_resource
#' @param df A data frame.
#' @param schema A Table Schema for the Data Resource. If not provided, one
#' will be created (using [create_schema()]).
#' @return Provided `package` with one additional resource.
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
#' # Add the data frame as a new resource to the Data Package
#' package <- add_resource(package, "positions", df)
#'
#' # List the resource names ("positions" added)
#' package$resource_names
add_resource <- function(package, resource_name, df, schema = NULL) {
  # Check package
  check_package(package)

  # Check resource name
  assertthat::assert_that(
    grepl(resource_name, pattern = "^[a-z0-9\\._-]+$"),
    msg = glue::glue(
      "`{resource_name}` must only contain lowercase alphanumeric characters",
      "plus `.`, `-` and `_`.", .sep = " "
    )
  )

  # Check resource is absent
  assertthat::assert_that(
    !resource_name %in% package$resource_names,
    msg = glue::glue(
      "`package` already contains a resource named `{resource_name}`."
    )
  )

  # Check df
  assertthat::assert_that(
    is.data.frame(df) &
    dim(df)[1] != 0 & # Must have rows
    dim(df)[2] != 0,  # Must have columns
    msg = glue::glue(
      "`df` must be a data frame containing data."
    )
  )

  # Check schema
  if (is.null(schema)) {
    out_schema <- create_schema(df)
  } else {
    out_schema <- schema
  }

  # Create resource
  resource <- list(
    name = resource_name,
    data = df,
    profile = "tabular-data-resource", # Necessary for read_resource()
    # other properties are set by write_resource()
    schema = out_schema
  )

  # Add resource (needs to be wrapped in its own list)
  package$resources <- append(package$resources, list(resource))

  # Add resource_name
  package$resource_names <- append(package$resource_names, resource_name)

  package
}
