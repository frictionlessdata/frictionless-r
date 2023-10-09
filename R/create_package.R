#' Create an empty Data Package
#'
#' Initiates a list describing a [Data
#' Package](https://specs.frictionlessdata.io/data-package/).
#' This empty Data Package can be extended with metadata and resources (see
#' [add_resource()]).
#' Added resources will make the Data Package meet [Tabular Data
#' Package](https://specs.frictionlessdata.io/tabular-data-package/)
#' requirements, so `profile` is set to `tabular-data-package`.
#'
#' @return List describing a Data Package.
#' @family create functions
#' @export
#' @examples
#' # Create a Data Package
#' package <- create_package()
#' str(package)
create_package <- function() {
  descriptor <- list(
    profile = "tabular-data-package",
    resources = list(),
    directory = "." # Current directory
  )
  class(descriptor) <- c("datapackage", class(descriptor))
  descriptor
}
