#' Upgrade a Table Schema object from v1 to v2
#'
#' @param schema List describing a Table Schema, as returned by [schema()].
#' @param resource_name Name of the Data Resource, needed for updating
#'   `foreignKeys`.
#' @returns Upgraded `schema`.
#' @family upgrade functions
#' @noRd
upgrade_schema <- function(schema, resource_name) {
  version <- version(schema)

  # Leave schema as is if already 2.0 or higher
  if (version != "1.0") {
    return(schema)
  }

  # Set $schema
  purrr::pluck(schema, "$schema") <-
    "https://datapackage.org/profiles/2.0/tableschema.json"

  # Update primaryKey
  primaryKey <- purrr::pluck(schema, "primaryKey")
  if (!is.null(primaryKey)) {
    schema$primaryKey <- list(primaryKey) # Wrap in list to ensure array
  }
  # https://datapackage.org/overview/changelog/#schemaprimarykey-updated
  # https://datapackage.org/overview/changelog/#schemaforeignkeys-updated

  # Update foreignKeys
  foreignKeys <- purrr::pluck(schema, "foreignKeys")
  if (!is.null(foreignKeys)) {
    schema$foreignKeys <- purrr::map(foreignKeys, function(key) {
      foreignKeys <- list(
        fields = list(key$fields), # Wrap in list to ensure array
        reference = list(
          resource = key$reference$resource,
          fields = list(key$reference$fields) # Wrap in list to ensure array
        )
      )
      # Remove foreignKeys.reference[x].resource if equal to resource name
      if (key$reference$resource == resource_name) {
        foreignKeys$reference$resource <- NULL
      }
      return(foreignKeys)
    })
  }
  return(schema)
}
