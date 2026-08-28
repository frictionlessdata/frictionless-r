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

  # Set $schema as first property
  package <- append_with_attributes(
    package,
    list("$schema" = "https://datapackage.org/profiles/2.0/datapackage.json"),
    after = 0
  )

  # Set $schema to profile if URL (to custom profile)
  # https://datapackage.org/standard/data-package/#dollar-schema
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
  package$profile <- NULL

  # Update contributor "role" = "value" to "roles" = ["value"]
  # https://datapackage.org/overview/changelog/#packagecontributors-updated
  package$contributors <- purrr::modify_if(
    package$contributors,
    function(x) "role" %in% names(x),
    function(x) purrr::list_modify(
      x,
      roles = character_to_list(x$role),
      role = purrr::zap()
    )
  )

  return(package)
}
