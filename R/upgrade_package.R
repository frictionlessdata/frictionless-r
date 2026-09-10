#' Upgrade a Data Package from v1 to v2
#'
#' ...
#'
#' @inheritParams read_resource
#' @returns Upgraded `package`.
#' @family versioning functions
#' @export
#' @examples
#' # Load the v1 example Data Package
#' (package <- example_package(version = "1.0"))
#'
#' # Upgrade
#' (package_upgraded <- upgrade_package(package))
#'
#' # Check version of one of the resources
#' version(resource(package_upgraded, "deployments"))
upgrade_package <- function(package) {
  check_package(package)

  # Upgrade descriptor
  package <- upgrade_descriptor(package)

  # Upgrade resources
  for (resource_name in resource_names(package)) {
    resource <- upgrade_resource(resource(package, resource_name))

    # Upgrade schema if verbosely included
    schema_property <- resource$schema %||% "undefined"
    if (is.list(schema_property)) {
      schema <- schema(package, resource_name)
      schema <- upgrade_schema(schema, resource_name)
      resource$schema <- schema
    }
    resource(package, resource_name) <- resource
  }

  return(package)
}
