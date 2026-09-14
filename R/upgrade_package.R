#' Upgrade a Data Package to v2
#'
#' Upgrades a Data Package, its Data Resources and Table Schemas to the
#' [v2](https://datapackage.org/) specification.
#'
#' @section Upgrade details:
#'
#' ### Data Package
#'
#' `upgrade_package()` upgrades a [v1](
#' https://specs.frictionlessdata.io/data-package/) descriptor to [v2](
#' https://datapackage.org/standard/data-package/) as follows:
#'
#' - Adds `$schema` as first property and sets it to the recommended v2 value
#'   (`"https://datapackage.org/profiles/2.0/datapackage.json"`), except for
#'   certain `profile` values.
#' - Removes `profile`, but retains its value in `$schema` if it is a URL to a
#'   custom profile (see [backwards compatibility](
#'   https://datapackage.org/standard/data-package/#dollar-schema)).
#' - Converts `contributors` `"role": "value"` to `"roles": ["value"]` (see
#'   [changelog](
#'   https://datapackage.org/overview/changelog/#packagecontributors-updated)).
#'
#' ### Data Resource
#'
#' `upgrade_package()` upgrades any [v1](
#' https://specs.frictionlessdata.io/data-resource/) resource to [v2](
#' https://datapackage.org/standard/data-resource/) as follows:
#'
#' - Adds `$schema` as first property and sets it to the recommended v2 value
#'   (`"https://datapackage.org/profiles/2.0/dataresource.json"`).
#' - Removes `profile`, but converts `"profile" = "tabular-data-resource"` to
#'   `"type" = "table"` (see [backwards compatibility](
#'   https://datapackage.org/standard/data-resource/#type)).
#'
#' ### Table Dialect
#'
#' `upgrade_package()` leaves the dialect as is for all resources.
#'
#' ### Table Schema
#'
#' `upgrade_package()` upgrades any [v1](
#' https://specs.frictionlessdata.io/table-schema/) schema to [v2](
#' https://datapackage.org/standard/table-schema/) as follows:
#'
#' - Adds `$schema` as first property and sets it to the recommended v2 value
#'   (`"https://datapackage.org/profiles/2.0/tableschema.json"`).
#' - Converts `primaryKey` single values to an array (see [changelog](
#'   https://datapackage.org/overview/changelog/#schemaprimarykey-updated)).
#' - Converts `foreignKeys` single values in `fields` to an array and removes
#'   `reference$resource` if it is self-referential (see [changelog](
#'   https://datapackage.org/overview/changelog/#schemaforeignkeys-updated)).
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
