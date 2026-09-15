#' @export
add_properties <- function(x, ..., after = length(x)) {
  # Check dots
  properties <- check_dots(...)

  # Keep original attributes of x, except names (length will change with append)
  original_attributes <- attributes(x)
  original_attributes[["names"]] <- NULL

  # Append properties (recreates names)
  x <- base::append(unclass(x), properties, after = after)

  # Add original attributes
  for (attribute_name in names(original_attributes)) {
    attr(x, attribute_name) <- original_attributes[[attribute_name]]
  }

  return(x)
}

