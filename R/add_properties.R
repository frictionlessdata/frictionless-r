#' Add properties
#'
#' Adds or inserts provided arguments as elements to a list.
#' Attributes and classes of the original list are retained.
#'
#' @details
#' `add_properties()` can be used to add (custom) metadata properties to a Data
#' Package, Data Resource, Table Dialect or Table Schema.
#' Note that added properties are not validated.
#' See the vignettes for an overview of all standard metadata properties.
#'
#' `add_properties()` has some advantages over:
#'
#' - [append()]: removes attributes and classes, invalidating a Data Package
#'   object.
#' - Direct assignment (`package$title <- "My package"`): no control over the
#'   position of the added element.
#'
#' `add_resource()` also supports adding metadata properties to a Data Resource,
#' except those automatically set by that function.
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
#' package <- add_properties(
#'   package,
#'   title = "Example package",
#'   keywords = c("camera traps", "frictionlessdata"),
#'   after = 1 # Add after the first property
#' )
#' package
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

