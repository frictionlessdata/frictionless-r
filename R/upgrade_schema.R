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

  # Update primaryKey to array
  # https://datapackage.org/overview/changelog/#schemaprimarykey-updated
  schema$primaryKey <- character_to_list(schema$primaryKey)

  # Update foreignKeys fields to array and remove self-referential resource
  # https://datapackage.org/overview/changelog/#schemaforeignkeys-updated
  schema$foreignKeys <- purrr::modify_if(
    schema$foreignKeys,
    is.list,
    function(x) {
      if (!is.null(purrr::pluck(x, "fields"))) {
        x$fields <- character_to_list(x$fields)
      }
      if (!is.null(purrr::pluck(x, "reference", "fields"))) {
        x$reference$fields <- character_to_list(x$reference$fields)
      }
      if (!is.null(purrr::pluck(x, "reference", "resource"))) {
        if (x$reference$resource == resource_name) {
          x$reference$resource <- NULL
        }
      }
      x
    }
  )

  return(schema)
}
