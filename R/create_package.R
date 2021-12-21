#' Create an empty Data Package
#'
#' Initiates a list object describing a [Data
#' Package](https://specs.frictionlessdata.io/data-package/).
#' This empty Data Package can then be extended with metadata and resources (see
#' [add_resource()]).
#'
#' @return List object describing a Data Package.
#' @family create functions
#' @export
#' @examples
#' # Create a Data Package
#' package <- create_package()
#' str(package)
create_package <- function() {
  descriptor <- list(
    resources = list(),
    resource_names = vector(mode = "character"),
    directory = "." # Current directory
  )

  # Add datapackage class
  class(descriptor) <- c("datapackage", class(descriptor))

  descriptor
}
