# UPGRADE HELPER FUNCTIONS

#' Upgrade a package object from v1 to v2
#'
#' Upgrades the descriptor, not the associated resources, etc.
#'
#' @inheritParams read_resource
#' @returns Upgraded `package`.
#' @family upgrade functions
#' @noRd
upgrade_descriptor <- function(package) {
  version <- version(package)

  # Leave descriptor as is if already 2.0 or higher
  if (version != "1.0") {
    return(package)
  }

  # Set $schema
  purrr::pluck(package, "$schema") <-
    "https://datapackage.org/profiles/2.0/datapackage.json"

  # Set $schema to profile if URL (to custom profile)
  profile <- package$profile %||% "undefined"
  if (is_url(profile)) {
    purrr::pluck(package, "$schema") <- profile
  }

  # Set $schema profile if "fiscal-data-package" and set to its URL
  if (profile == "fiscal-data-package") {
    purrr::pluck(package, "$schema") <-
      "https://specs.frictionlessdata.io/schemas/fiscal-data-package.json"
  }

  # Remove profile
  resource$profile <- NULL

  # Update contributor roles
  contributors <- purrr::pluck(package, "contributor", .default = NULL)
  ## Only update if there actually are any contributors
  if(!is.null(contributors)){
    purrr::pluck(package, "contributors") <-
      purrr::map(
        contributors,
        \(ctb){
          # Support both role and other values (roles):
          if ("role" %in% names(ctb)) {
            purrr::list_modify(
              ctb,
              roles = as.list(ctb$role),
              role = purrr::zap()
            )
          } else {
            # If contributors do not have a role, return them as is.
            ctb
          }
        }
      )
  }
}

#' Upgrade a Data Resource object from v1 to v2
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

  # Remove profile, including URL to custom *resource* profile, but hardly used
  resource$profile <- NULL

  return(resource)
}
