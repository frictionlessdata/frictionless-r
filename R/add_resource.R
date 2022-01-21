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
#' @param data Data to attach, either a data frame or path(s) to CSV file(s):
#'   - Data frame: attached to the resource as `data` and written to a CSV file
#'     when using [write_package()].
#'   - One or more paths to CSV file(s) as a character (vector): added to the
#'     resource as `path`.
#'     The **last file will be read** with [readr::read_delim()] to create or
#'     compare with `schema` and to set `format`, `mediatype` and `encoding`.
#'     The other files are ignored, but are expected to have the same structure
#'     and properties.
#' @param schema Either a list, or path or URL to a JSON file describing a Table
#'   Schema for the `data`.
#'   If not provided, one will be created using [create_schema()].
#' @param delim Single character used to separate the fields in the CSV file(s),
#'   e.g. `\t` for tab delimited file.
#'   Will be set as `delimiter` in the resource [CSV
#'   dialect](https://specs.frictionlessdata.io/csv-dialect/#specification), so
#'   read functions know how to read the file(s).
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
#' # Add resource "positions" to the Data Package, from the data frame
#' package <- add_resource(package, "positions", data = df)
#'
#' # Add resource "positions2" to the Data Package, with user-defined schema
#' my_schema <- create_schema(df)
#' package <- add_resource(package, "positions2", data = df, schema = my_schema)
#'
#' # Add resource "observations2" to the Data Package, from CSV file paths
#' path_1 <- system.file("extdata", "observations_1.csv", package = "frictionless")
#' path_2 <- system.file("extdata", "observations_2.csv", package = "frictionless")
#' package <- add_resource(package, "observations2", data = c(path_1, path_2))
#'
#' # List the resource names ("positions", "positions2" & "observations2" added)
#' package$resource_names
add_resource <- function(package, resource_name, data, schema = NULL,
                         delim = ",") {
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
    # Check existence of files (no further checks) and read last file
    paths <- purrr::map_chr(data, ~ check_path(.x, safe = FALSE))
    last_file <- utils::tail(paths, n = 1)
    extension <- utils::tail(strsplit(last_file, "\\.")[[1]], n = 1)
    encoding <- readr::guess_encoding(last_file, n_max = 1000)[[1, 1]]
    encoding <- replace_null(encoding, "UTF-8")
    df <- readr::read_delim(
      file = paths[length(paths)],
      delim = delim,
      show_col_types = FALSE,
    )
  }

  # Create schema
  if (is.null(schema)) {
    schema <- create_schema(df)
  } else if (is.character(schema)) {
    # Path to schema can be unsafe, since schema will be verbosely included
    schema <- read_descriptor(schema, safe = FALSE)
  }

  # Check schema (also checks df)
  check_schema(schema, df)

  # Create resource, with properties in specific order
  if (is.data.frame(data)) {
    resource <- list(
      name = resource_name,
      data = df,
      profile = "tabular-data-resource", # Necessary for read_resource()
      # other properties are set by write_resource()
      schema = schema
    )
  } else {
    resource <- list(
      name = resource_name,
      path = paths,
      profile = "tabular-data-resource", # Necessary for read_resource()
      format = extension,
      mediatype = ifelse(
        delim == "\t",
        "text/tab-separated-values",
        "text/csv"
      ),
      encoding = ifelse(encoding == "ASCII", "UTF-8", encoding),
      schema = schema
    )
    # Add CSV dialect for non-default delimiter
    if (delim != ",") {
      resource$dialect = list(delimiter = delim)
    }
    # Set attribute for get_resource()
    attr(resource, "path") <- "added"
  }

  # Add resource (needs to be wrapped in its own list)
  package$resources <- append(package$resources, list(resource))

  # Add resource_name
  package$resource_names <- append(package$resource_names, resource_name)

  package
}
