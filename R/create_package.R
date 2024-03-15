#' Create an empty Data Package
#'
#' Initiates a [Data Package]
#' (https://specs.frictionlessdata.io/data-package/), a list with a
#' `datapackage` class.
#' This empty Data Package can be extended with metadata and resources (see
#' [add_resource()]).
#' Added resources will make the Data Package meet [Tabular Data
#' Package](https://specs.frictionlessdata.io/tabular-data-package/)
#' requirements, so `profile` is set to `tabular-data-package`.
#'
#' @return Data Package object.
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

  # Add datapackage class
  class(descriptor) <- c("datapackage", class(descriptor))
}
