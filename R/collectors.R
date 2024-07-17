#' Parse a string field
#'
#' @param enum A field's `constraints.enum`.
#' @return A readr collector.
#' @family parse functions
#' @noRd
col_string <- function(enum) {
  if (length(enum) > 0) {
    readr::col_factor(levels = enum)
  } else {
    readr::col_character()
  }
}

#' Parse a number field
#'
#' @param enum A field's `constraints.enum`.
#' @param group_char A field's `groupChar`.
#' @param bare_number A field's `bareNumber`.
#' @return A readr collector.
#' @family parse functions
#' @noRd
col_number <- function(enum, group_char, bare_number) {
  if (length(enum) > 0) {
    readr::col_factor(levels = as.character(enum))
  } else if (group_char) {
    readr::col_number() # Supports grouping_mark
  } else if (bare_number) {
    readr::col_double() # Allows NaN, INF, -INF
  } else {
    readr::col_number() # Strips non-num. chars, uses default grouping_mark
  }
}

#' Parse an integer field
#'
#' @param enum A field's `constraints.enum`.
#' @param bare_number A field's `bareNumber`.
#' @return A readr collector.
#' @family parse functions
#' @noRd
col_integer <- function(enum, bare_number) {
  if (length(enum) > 0) {
    readr::col_factor(levels = as.character(enum))
  } else if (bare_number) {
    readr::col_double() # Not col_integer() to avoid big integers issues
  } else {
    readr::col_number() # Strips non-numeric chars
  }
}

#' Parse a date field
#'
#' @param format A field's `format`.
#' @return A readr collector.
#' @family parse functions
#' @noRd
col_date <- function(format) {
  readr::col_date(
    format = switch(
      format,
      "default" = "%Y-%m-%d", # ISO
      "any" = "%AD", # YMD
      "%x" = "%m/%d/%y", # Python strptime for %x
      format # Default
    )
  )
}

#' Parse a time field
#'
#' @param format A field's `format`.
#' @return A readr collector.
#' @family parse functions
#' @noRd
col_time <- function(format) {
  readr::col_time(
    format = switch(
      format,
      "default" = "%AT", # H(MS)
      "any" = "%AT", # H(MS)
      "%X" = "%H:%M:%S", # HMS
      sub("%S.%f", "%OS", format) # Default, use %OS for milli/microseconds
    )
  )
}

#' Parse a datetime field
#'
#' @param format A field's `format`.
#' @return A readr collector.
#' @family parse functions
#' @noRd
col_datetime <- function(format) {
  readr::col_datetime(
    format = switch(
      format,
      "default" = "", # ISO (lenient)
      "any" = "", # ISO (lenient)
      sub("%S.%f", "%OS", format) # Default, use %OS for milli/microseconds
    )
  )
}
