# HELPER FUNCTIONS

#' Replace value when NULL
#'
#' @param value Value to test.
#' @param replacement Replacement value when `value` is `NULL`.
#'
#' @return `value` when not `NULL`, otherwise `replacement`.
#'
#' @keywords internal
replace_null <- function(value, replacement) {
  if(!is.null(value)) { value } else { replacement }
}

#' Get unique vector values sorted by how often they occur
#'
#' @param x Vector, e.g. `c("a", "b", "b", "b", "a")`.
#'
#' @return Vector with unique values sorted by occurrence, e.g. `c("b", "a")`.
#'
#' @keywords internal
#'
#' @importFrom dplyr arrange desc pull %>%
unique_sorted <- function(x) {
  stats::aggregate(x, by = list(x), FUN = length) %>%
    arrange(desc(x)) %>%
    pull("Group.1")
}
