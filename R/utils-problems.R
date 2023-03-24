#' Retrieve parsing problems
#'
#' Allows to show parsing issues in data frame returned by [read_resource()].
#' See [readr::problems()] for details.
#'
#' @name problems
#' @importFrom readr problems
#' @export
#' @keywords internal
# This function is added to the NAMESPACE so users don't have to load readr.
# Inspired by usethis::use_pipe()
# Required to have .rd file, but not listed in function reference (internal).
NULL
