#' Add properties
#'
#' Adds provided arguments as elements to a list.
#' Unlike [append()] the original attributes and classes are retained.
#' Can be used to add (custom) properties to a Data Package, Data Resource,
#' Table Dialect or Table Schema.
#'
#' @details
#' You can also use [add_resource()] to add properties to a Data Resource.
#'
#' @param x A list or vector.
#' @param ... Elements to add, as named arguments.
#' @param after Position after which elements should be added.
#'   Use `0` to add elements at the beginning.
#' @returns `x` with the added elements.
#' @family edit functions
#' @export
#' @examples
#' # Add property to a generic list
#' list <- list(a = 1, b = 2)
#' add_properties(list, added = 3)
#'
#' # Add properties to a Data Package
#' package <- create_package()
#' (package <- add_properties(
#'   package,
#'   title = "Example package",
#'   keywords = c("example", "camera trap data"),
#'   after = 1 # After first property
#' ))
#'
#' str(package)
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

