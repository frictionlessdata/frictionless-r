#' Create an empty Data Package
#'
#' Initiates a [Data Package](https://specs.frictionlessdata.io/data-package/)
#' descriptor as a list object of class `datapackage` (cf. [read_package()]).
#' Can be extended with metadata and resources (see _add_resource()_).
#'
#' @return List object describing a Data Package.
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
