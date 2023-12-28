#' Check data
#'
#' Check if an object is a non-empty data frame.
#'
#' @param data A data frame.
#' @return `TRUE` or error.
#' @family check functions
#' @noRd
check_data <- function(data) {
  if (
    !is.data.frame(data) ||
    replace_null(dim(data)[1], 0) == 0 ||
    replace_null(dim(data)[2], 0) == 0
  ) {
    cli::cli_abort(
      "{.arg data} must be a data frame containing data.",
      class = "frictionless_error_data_incorrect"
    )
  }

  # Return TRUE
  TRUE
}
