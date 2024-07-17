#' Create column type for reading a string field of a Data Resource.
#'
#' Create a readr column type for reading a string field.
#'
#' @param enum Enumerated values, if any.
#' @return A [readr::col_factor()] or a [readr::col_character()] object.
#' @family helper functions
#' @noRd
col_string <- function(enum) {
  if (length(enum) > 0) {
    readr::col_factor(levels = enum)
  } else {
    readr::col_character()
  }
}

#' Create column type for reading a number field of a Data Resource.
#'
#' Create a readr column type for reading a number field.
#'
#' @param enum Enumerated values, if any.
#' @param group_char Whether to use [readr::col_number()], which supports
#'   grouping marks.
#' @param bare_number Whether to use [readr::col_double()], which allows NaN,
#'   INF and -INF.
#' @return A [readr::col_factor()], a [readr::col_number()], or a
#'   [readr::col_double] object.
#' @family helper functions
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

#' Create column type for reading an integer field of a Data Resource.
#'
#' Create a readr column type for reading an integer field.
#'
#' @param enum Enumerated values, if any.
#' @param bare_number Whether to use [readr::col_double()]. This is done to
#'  avoid issues with big integers.
#' @return A [readr::col_factor()], a [readr::col_double] object or a
#'   [readr::col_number()] object.
#' @family helper functions
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

#' Create column type for reading a date field of a Data Resource.
#'
#' Create a readr column type for reading a date field.
#'
#' @param format Datetime format.
#' @return A [readr::col_date()] object.
#' @family helper functions
#' @noRd
col_date <- function(format) {
  readr::col_date(format = switch(format,
                                  "default" = "%Y-%m-%d", # ISO
                                  "any" = "%AD", # YMD
                                  "%x" = "%m/%d/%y", # Python strptime for %x
                                  format # Default
  ))
}

#' Create column type for reading a time field of a Data Resource.
#'
#' Create a readr column type for reading a time field.
#'
#' @param format Datetime format.
#' @return A [readr::col_time()] object.
#' @family helper functions
#' @noRd
col_time <- function(format) {
  readr::col_time(format = switch(format,
                                  "default" = "%AT", # H(MS)
                                  "any" = "%AT", # H(MS)
                                  "%X" = "%H:%M:%S", # HMS
                                  sub("%S.%f", "%OS", format) # Default, use %OS for milli/microseconds
  ))
}

#' Create column type for reading a datetime field of a Data Resource.
#'
#' Create a readr column type for reading a datetime field.
#'
#' @param format Datetime format.
#' @return A [readr::col_datetime()] object.
#' @family helper functions
#' @noRd
col_datetime <- function(format) {
  readr::col_datetime(format = switch(format,
                                      "default" = "", # ISO (lenient)
                                      "any" = "", # ISO (lenient)
                                      sub("%S.%f", "%OS", format) # Default, use %OS for milli/microseconds
  ))
}

