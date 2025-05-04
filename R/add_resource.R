#' Add a Data Resource
#'
#' Adds a Data Resource to a Data Package.
#' The resource will be a [Tabular Data Resource](
#' https://specs.frictionlessdata.io/tabular-data-resource/).
#' The resource name can only contain lowercase alphanumeric characters plus
#' `.`, `-` and `_`.
#'
#' See `vignette("data-resource")` (and to a lesser extend
#' `vignette("table-dialect")`) to learn how this function implements the
#' Data Package standard.
#'
#' @inheritParams read_resource
#' @param data Data to attach, either a data frame or path(s) to CSV file(s):
#'   - Data frame: attached to the resource as `data` and written to a CSV file
#'     when using [write_package()].
#'   - One or more paths to CSV file(s) as a character (vector): added to the
#'     resource as `path`.
#'     The last file will be read with [readr::read_delim()] to create or
#'     compare with `schema` and to set `format`, `mediatype` and `encoding`.
#'     The other files are ignored, but are expected to have the same structure
#'     and properties.
#' @param schema Either a list, or path or URL to a JSON file describing a Table
#'   Schema for the `data`.
#'   If not provided, one will be created using [create_schema()].
#' @param replace If `TRUE`, the added resource will replace an existing
#'   resource with the same name.
#' @param delim Single character used to separate the fields in the CSV file(s),
#'   e.g. `\t` for tab delimited file.
#'   Will be set as `delimiter` in the resource Table Dialect, so read functions
#'   know how to read the file(s).
#' @param ... Additional [metadata properties](
#'   https://docs.ropensci.org/frictionless/articles/data-resource.html#properties-implementation)
#'   to add to the resource, e.g. `title = "My title", validated = FALSE`.
#'   These are not verified against specifications and are ignored by
#'   [read_resource()].
#'   The following properties are automatically set and can't be provided with
#'   `...`: `name`, `data`, `path`, `schema`, `profile`, `format`, `mediatype`,
#'   `encoding` and `dialect`.
#' @return `package` with one additional resource.
#' @family edit functions
#' @export
#' @examples
#' # Load the example Data Package
#' package <- example_package()
#'
#' # List the resources
#' resources(package)
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
#' # Add the resource "positions" from the data frame
#' package <- add_resource(package, "positions", data = df)
#'
#' # Add the resource "positions_with_schema", with a user-defined schema and title
#' my_schema <- create_schema(df)
#' package <- add_resource(
#'   package,
#'   resource_name = "positions_with_schema",
#'   data = df,
#'   schema = my_schema,
#'   title = "Positions with schema"
#' )
#'
#' # Replace the resource "observations" with a file-based resource (2 TSV files)
#' path_1 <-
#'   system.file("extdata", "v1", "observations_1.tsv", package = "frictionless")
#' path_2 <-
#'   system.file("extdata", "v1", "observations_2.tsv", package = "frictionless")
#' package <- add_resource(
#'   package,
#'   resource_name = "observations",
#'   data = c(path_1, path_2),
#'   replace = TRUE,
#'   delim = "\t"
#' )
#'
#' # List the resources ("positions" and "positions_with_schema" added)
#' resources(package)
add_resource <- function(package, resource_name, data, schema = NULL,
                         replace = FALSE, delim = ",", ...) {
  # Check package
  check_package(package)

  # Check resource name
  if (!grepl(resource_name, pattern = "^[a-z0-9\\._-]+$")) {
    cli::cli_abort(
      c(
        "{.arg resource_name} must only consist of lowercase alphanumeric
         characters, {.val .}, {.val -} and {.val _}.",
        "x" = "{.val {resource_name}} does not meet those criteria."
      ),
      class = "frictionless_error_resource_name_invalid"
    )
  }

  # Check if replace is a logical value
  if (!is.logical(replace)) {
    cli::cli_abort(
      "{.arg replace} must be a logical value.",
      class = "frictionless_error_replace_invalid"
    )
  }

  # Check resource does not exist yet for replace = FALSE
  if (!replace && resource_name %in% resources(package)) {
    cli::cli_abort(
      c(
        "{.arg package} already contains a resource named
        {.val {resource_name}}.",
        "i" = "Use {.arg replace = TRUE} to replace an existing resource."
      ),
      class = "frictionless_error_resource_already_exists"
    )
  } else if (replace && !(resource_name %in% resources(package))) {
    replace <- FALSE
  }

  # Check data (data frame or path), content of data frame is checked later
  if (!is.data.frame(data) && !is.character(data)) {
    cli::cli_abort(
      "{.arg data} must either be a data frame or path(s) to CSV file(s).",
      class = "frictionless_error_data_type_invalid"
    )
  }

  if (is.data.frame(data)) {
    df <- data
  } else {
    # Check existence of files (no further checks) and read last file
    paths <- purrr::map_chr(data, ~ check_path(.x, safe = FALSE))
    last_file <- utils::tail(paths, n = 1)
    encoding <- readr::guess_encoding(last_file, n_max = 1000)[[1, 1]]
    df <- readr::read_delim(
      file = last_file,
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

  # Check ellipsis
  if (length(list(...)) != length(get_dot_names(...))) {
    cli::cli_abort(
      "All arguments in {.arg ...} must be named.",
      class = "frictionless_error_argument_unnamed"
    )
  }
  properties <- get_dot_names(...)
  reserved_properties <- c(
    "name", "path", "profile", "format", "mediatype", "encoding", "dialect"
  ) # data and schema are also reserved, but are named arguments
  conflicting_properties <- properties[properties %in% reserved_properties]
  if (length(conflicting_properties) != 0) {
    cli::cli_abort(
      c(
        "{.arg {conflicting_properties}} must be removed as argument{?s}.",
        "i" = "{.field {conflicting_properties}} {?is/are} automatically added
               as resource propert{?y/ies}."
      ),
      class = "frictionless_error_resource_properties_reserved"
    )
  }

  # Create resource, with properties in specific order
  if (is.data.frame(data)) {
    resource <- list(
      name = resource_name,
      data = df,
      profile = "tabular-data-resource", # Necessary for read_resource()
      format = NULL, # Will be set with write_resource()
      mediatype = NULL,
      encoding = NULL,
      dialect = NULL,
      ...,
      schema = schema
    )
  } else {
    resource <- list(
      name = resource_name,
      path = paths,
      profile = "tabular-data-resource", # Necessary for read_resource()
      format = if (delim == "\t") "tsv" else "csv",
      mediatype = if (delim == "\t") "text/tab-separated-values" else "text/csv",
      encoding = if (encoding == "ASCII") "UTF-8" else encoding, # UTF-8 = safer
      dialect = NULL,
      ...,
      schema = schema
    )
    # Add CSV dialect for non-default delimiter or remove it
    resource$dialect <- if (delim != ",") list(delimiter = delim) else NULL

    # Set attribute for get_resource()
    attr(resource, "path") <- "added"
  }

  # Add or replace resource (needs to be wrapped in its own list)
  if (replace) {
    index <- which(purrr::map(package$resources, "name") == resource_name)
    package$resources[index] <- list(resource)
  } else {
    package$resources <- append(package$resources, list(resource))
  }

  return(package)
}
