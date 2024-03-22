#' Print a Data Package
#'
#' Prints a human-readable summary of a Data Package, including its resources
#' and a link to more information (if provided in `package$id`).
#'
#' @inheritParams read_resource
#' @return [print()] with a summary of the Data Package object.
#' @family print functions
#' @export
#' @examples
#' # Load the example Data Package
#' package <- example_package
#'
#' # Print a summary of the Data Package
#' package # Or print(package)
print.datapackage <- function(package) {
  # List resources
  resources <- resources(package)
  cli::cli_text(
    "A Data Package with {length(resources)} resource{?s}{?./: /: }"
  )
  cli::cli_bullets(cli::cat_bullet(resources, bullet = "bullet"))

  # Include link (DOI) if available in package$id
  if (startsWith(replace_null(package$id, ""), "http")) {
    cli::cli_text(
      "For more information, see {.url {package$id}}."
    )
  }

  # Provide help
  cli::cli_text(
    cli::col_silver(
      "Use {.fun as.list} to see the metadata and
       {.fun read_resource} to load data from a resource."
    )
  )
}
