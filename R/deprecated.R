#' Deprecated functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Use [schema()] instead of `get_schema()`.
#'
#' @family deprecated functions
#' @export
#' @keywords internal
#' @name deprecated
get_schema <- function(package, resource_name) {
  lifecycle::deprecate_soft(
    when = "1.3.0",
    what = "get_schema()",
    with = "schema()"
  )
  schema(package, resource_name)
}

#' @description
#' Use [resource_names()] instead of `resources()`.
#'
#' @family deprecated functions
#' @export
#' @keywords internal
#' @name deprecated
resources <- function(package) {
  lifecycle::deprecate_soft(
    when = "1.3.0",
    what = "resources()",
    with = "resource_names()"
  )
  resource_names(package)
}
