#' @export
append <- function(x, values, after = length(x)) {
  # This function needs to be exported or base::append() will stop working when
  # loading the package. Because base append() is not an S3 generic, we need to
  # define one to dispatch to append.datapackage() or append.default().
  UseMethod("append")
}

#' @exportS3Method
append.default <- function(x, values, after = length(x)) {
  base::append(x, values, after = after)
}

#' Append a Data Package
#'
#' Appends values to a Data Package object, preserving its attributes such as
#' directory and class.
#'
#' @param x Data Package object, as returned by [read_package()] or
#'   [create_package()].
#' @inheritParams base::append values after
#' @returns A Data Package object with the elements of `values` appended after
#'   the specified element of `x`.
#'
#' @exportS3Method
#'
#' @examples
#' p <- example_package()
#' p_app <- append(p, list("$schema" = "just added"), after = 0)
#' p_app
#' attributes(p_app)
append.datapackage <- function(x, values, after = length(x)){
  attrs <- attributes(x)
    # Names will be the wrong length when appending
  attrs[["names"]] <- NULL
  p_app <- base::append(unclass(x), values, after = after)

  attributes(p_app) <- purrr::list_modify(
    # Start from the newly appended attributes
    attributes(p_app),
    # Add our existing attributes: directory and class
    val = !!!attrs
  )
  p_app
}
