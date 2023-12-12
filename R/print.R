#' Print a data package
#'
#' @param x Data package to print
#' @family print functions
#' @export
#' @examples
#' # Read a datapackage.json file
#' package <- read_package(
#'   system.file("extdata", "datapackage.json", package = "frictionless")
#' )
#'
#' # Print out a summary of the data package
#' print(package)
print.datapackage <- function(x) {
  name <- replace_null(x$name, "(Unnamed)")

  tbl_resources <- purrr::keep(x$resources, \(r) r$profile == "tabular-data-resource")

  n_resources <- length(tbl_resources)

  cat(glue::glue("# A frictionless data package: '{name}'\n\n"))

  if (n_resources > 0) {
    cat(glue::glue("# {n_resources} available tabular data resource(s): \n\n\n"))

    rtable <- purrr::map(tbl_resources, function(r) {
      tibble::tibble(
        name = r$name,
        fields = length(r$schema$fields),
        description = stringr::str_trunc(replace_null(r$description, "~"), 20),
      )
    }) |>
      dplyr::bind_rows()

    print.data.frame(rtable)

    cat("\n")
    cat(
      glue::glue("# i Use `read_resource()` to load tabular data resource\n")
    )
  } else {
    cat("# No available tabular resources\n")
  }
}
