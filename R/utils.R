# HELPER FUNCTIONS

#' Get unique vector values sorted by how often they occur
#'
#' @param x Vector, e.g. `c("a", "b", "b", "b", "c", "a")`.
#' @returns Vector with unique values sorted by most to least occurring,
#'   e.g. `c("b", "a", "c")`.
#' @family helper functions
#' @noRd
unique_sorted <- function(x) {
  # Create table, sort on occurrence, return values (names)
  # c a b
  # 1 2 3
  values <- names(sort(table(x), decreasing = TRUE))
  # Return empty char vector if all values in x are NA, resulting in NULL
  values %||% character(0)
}

#' Clean list
#'
#' Removes all elements from a list that meet a criterion function, e.g.
#' [is.null()] for empty elements.
#' Removal can be recursive to guarantee elements are removed at any level.
#' Function is copied and adapted from `rlist::list.clean()` (MIT licensed), to
#' avoid requiring full `rlist` dependency.
#'
#' @param x List or vector.
#' @param fun Function returning `TRUE` for elements that should be removed.
#' @param recursive Whether list should be cleaned recursively.
#' @returns Cleaned list.
#' @family helper functions
#' @noRd
clean_list <- function(x, fun = is.null, recursive = FALSE) {
  if (recursive) {
    x <- lapply(x, function(item) {
      if (is.list(item)) {
        clean_list(item, fun, recursive = TRUE)
      } else {
        item
      }
    })
  }
  "[<-"(x, vapply(x, fun, logical(1L)), NULL)
}

#' Convert character to list
#'
#' @param x Value, e.g `"a"`.
#' @returns List containing value (`list("a")`) if character, otherwise value.
#' @family helper functions
#' @noRd
character_to_list <- function(x) {
  if (is.character(x)) as.list(x) else x
}

#' Check if path is URL
#'
#' @param path Path.
#' @returns `TRUE` if `path` is a http(s) or (s)ftp URL, otherwise `FALSE`.
#' @family helper functions
#' @noRd
is_url <- function(path) {
  grepl("^(http|https|ftp|ftps|sftp):\\/\\/", path)
}

#' Read descriptor
#'
#' Returns descriptor `x` as is, or attempts to read JSON/YAML from path or URL.
#'
#' @inheritParams check_path
#' @returns `x` (unchanged) or loaded JSON/YAML at path or URL.
#' @family helper functions
#' @noRd
read_descriptor <- function(x, directory = NULL, safe = FALSE) {
  # Return object
  if (!is.character(x)) {
    return(x)
  }

  # Read file
  x <- check_path(x, directory = directory, safe = safe)
  if (grepl("\\.yaml$", x) || grepl("\\.yml$", x)) {
    yaml::yaml.load_file(x)
  } else {
    # Default to jsonlite: better error messages for non .json files
    jsonlite::fromJSON(x, simplifyDataFrame = FALSE, simplifyVector = FALSE)
  }
}

#' Get names of arguments passed to ellipsis
#'
#' Replicates the base function [...names()] available in R >= 4.0.0.
#'
#' @param ... objects, possibly named
#' @returns A character vector of the names of the ... arguments
#' @noRd
get_dot_names <- function(...) {
  # Get all the names from ...
  dot_names <- names(list(...))
  # Return the names that are not an empty string (no name set)
  return(dot_names[dot_names != ""])
}

#' Vector merging while retaining attributes
#'
#' Expends [base::append] but retains attributes.
#'
#' @inheritParams base::append
#' @returns A vector containing the values in `x` with the elements of `values`
#'   appended after the specified element of `x`, while retaining the attributes
#'   of `x`
#' @noRd
append_with_attributes <- function(x, values, after = length(x)) {
  # Keep original attributes, except names, which length will change with append
  original_attributes <- attributes(x)
  original_attributes[["names"]] <- NULL

  # Append values (recreates names)
  x <- base::append(unclass(x), values, after = after)

  # Put original attributes back
  for (attribute_name in names(original_attributes)) {
    attr(x, attribute_name) <- original_attributes[[attribute_name]]
  }

  return(x)
}
