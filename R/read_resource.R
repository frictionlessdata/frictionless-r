#' Read data from a Data Resource into a tibble data frame
#'
#' Reads data from a Data Resource (in a Data Package) into a tibble (a
#' Tidyverse data frame).
#' The resource must be a [Tabular Data Resource](
#' https://specs.frictionlessdata.io/tabular-data-resource/).
#' The function uses [readr::read_delim()] to read CSV files, passing the
#' resource properties `path`, CSV dialect, column names, data types, etc.
#' Column names are taken from the provided Table Schema (`schema`), not from
#' the header in the CSV file(s).
#'
#' See `vignette("data-resource")`, `vignette("table-dialect")` and
#' `vignette("table-schema")` to learn how this function implements the
#' Data Package standard.
#'
#' @param package Data Package object, as returned by [read_package()] or
#'   [create_package()].
#' @param resource_name Name of the Data Resource.
#' @param col_select Character vector of the columns to include in the result,
#'   in the order provided.
#'   Selecting columns can improve read speed.
#' @return A [tibble::tibble()] with the Data Resource's tabular data.
#'   If there are parsing problems, a warning will alert you.
#'   You can retrieve the full details by calling [problems()] on your data
#'   frame.
#' @family read functions
#' @export
#' @examples
#' # Read a datapackage.json file
#' package <- read_package(
#'   system.file("extdata", "v1", "datapackage.json", package = "frictionless")
#' )
#'
#' package
#'
#' # Read data from the resource "observations"
#' read_resource(package, "observations")
#'
#' # The above tibble is merged from 2 files listed in the resource path
#' package$resources[[2]]$path
#'
#' # The column names and types are derived from the resource schema
#' purrr::map_chr(package$resources[[2]]$schema$fields, "name")
#' purrr::map_chr(package$resources[[2]]$schema$fields, "type")
#'
#' # Read data from the resource "deployments" with column selection
#' read_resource(package, "deployments", col_select = c("latitude", "longitude"))
read_resource <- function(package, resource_name, col_select = NULL) {
  # Get resource, includes check_package()
  resource <- resource(package, resource_name)

  # Read data directly
  if (resource$read_from == "df") {
    df <- dplyr::as_tibble(resource$data)

  # Read data from data
  } else if (resource$read_from == "data") {
    df <- do.call(
      function(...) rbind.data.frame(..., stringsAsFactors = FALSE),
      resource$data
    )
    df <- dplyr::as_tibble(df)

  # Read data from path(s)
  } else if (resource$read_from == "path" || resource$read_from == "url") {
    df <- read_from_path(package, resource_name, col_select)
  }
  return(df)
}
