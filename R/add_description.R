add_description <- function(old_schema, descriptions) {
  schema <- old_schema
  schema$fields <- purrr::imap(
    schema$fields,
    ~ c(.x, description = descriptions[.y])
  )
  return(schema)
}


edit_fields <- function(old_schema, metadata_name, metadata) {
  field_names <- names(metadata)
  schema <- old_schema
  for (name in field_names) {
    schema <- edit_field_from_name(schema, name, metadata_name, metadata[[name]])
  }
  return(schema)
}


edit_field_from_name <- function(old_schema, field_name, metadata_name, metadata) {
  field_names <- get_field_names(old_schema)
  schema <- old_schema
  names(schema$fields) <- field_names
  schema$fields[[field_name]][metadata_name] <- metadata
  names(schema$fields) <- NULL
  return(schema)
}


get_field_names <- function(old_schema) {
  purrr::map_chr(old_schema$fields, ~ .$name)
}
