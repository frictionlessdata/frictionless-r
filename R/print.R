#' Print a Data Package
#'
#' Prints a human-readable summary of a Data Package, including its resources
#' and a link to more information (if provided in `package$id`).
#'
#' @param x Data Package object, as returned by [read_package()] or
#'   [create_package()].
#' @param ... Further arguments, they are ignored by this function.
#' @return [print()] with a summary of the Data Package object.
#' @family print functions
#' @export
#' @examples
#' # Load the example Data Package
#' package <- example_package()
#'
#' # Print a summary of the Data Package
#' package # Or print(package)
print.datapackage <- function(x, ...) {
  # All prints should use cat (= cli::cat() helpers)

  # List resources
  resource_names <- resources(x)
  cli::cat_line(
    cli::format_inline(
      "A Data Package with {length(resource_names)} resource{?s}{?./:/:}"
    )
  )
  if (length(resource_names) > 0) {
    cli::cat_bullet(resource_names, bullet = "bullet")
  }

  # Include link (DOI) if available in package$id
  if (startsWith(x$id %||% "", "http")) {
    cli::cat_line(
      cli::format_inline("For more information, see {.url {x$id}}.")
    )
  }

  # Provide help
  cli::cat_line(
    cli::format_inline(
      "Use {.fun unclass} to print the Data Package as a list."
    ),
    col = "silver"
  )

  invisible(x)
}
