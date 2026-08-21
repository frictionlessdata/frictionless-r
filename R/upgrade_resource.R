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

  # Leave as is if already 2.0 or higher
  if (version != "1.0") {
    return(resource)
  }

  # Set $schema: copy URL to custom profile, otherwise define as generic 2.0
  profile <- resource$profile %||% ""
  if (is_url(profile)) {
    purrr::pluck(resource, "$schema") <- profile
  } else {
    purrr::pluck(resource, "$schema") <-
      "https://datapackage.org/profiles/2.0/dataresource.json"
  }

  # Set type if resource is tabular
  if (profile == "tabular-data-resource") {
    resource$type <- "table"
  }

  # Remove profile
  resource$profile <- NULL

  return(resource)
}
