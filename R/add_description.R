add_description <- function(old_schema, descriptions) {
  schema <- old_schema
  schema$fields <- purrr::imap(
    schema$fields,
    ~ c(.x, description = descriptions[.y])
  )
  return(schema)
}


edit_fields <- function(old_schema, metadata_name, metadata) {}


edit_field_from_name <- function(old_schema, field_name, metadata_name, metadata) {}


get_field_names <- function(old_schema) {
  comprehenr::to_vec(for (field in old_schema$fields) field$name)
}
