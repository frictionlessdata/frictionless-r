add_description <- function(old_schema, descriptions) {
  schema <- old_schema
  schema$fields <- purrr::imap(
    schema$fields,
    ~ c(.x, description = descriptions[.y])
  )
  return(schema)
}
