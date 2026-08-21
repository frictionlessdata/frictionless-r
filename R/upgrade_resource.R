#' Upgrade a Data Resource object
#'
#' Upgrades a Data Resource object from Data Package standard v1 to v2.
#'
#' @param resource List describing a Data Resource, as returned by [resource()].
#' @returns Upgraded `resource`.
#' @family upgrade functions
#' @noRd
upgrade_resource <- function(resource) {
  version <- version(resource)

  # Leave resource as is if already 2.0 or higher
  if (version != "1.0") {
    return(resource)
  }

  # Set $schema
  purrr::pluck(resource, "$schema") <-
    "https://datapackage.org/profiles/2.0/dataresource.json"

  # Set type to table if resource is tabular, see
  # https://datapackage.org/standard/data-resource/#type
  profile <- resource$profile %||% "undefined"
  if (profile %in% c(
    "tabular-data-resource",
    "https://specs.frictionlessdata.io/schemas/tabular-data-resource.json"
  )) {
    resource$type <- "table"
  }

  # Remove profile (https://specs.frictionlessdata.io/data-resource/#profile)
  # This may remove URL to custom resource profile, but is seldom-used feature
  resource$profile <- NULL

  return(resource)
}
