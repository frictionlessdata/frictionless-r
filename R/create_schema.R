#' Create a Table Schema for a data frame
#'
#' Creates a Table Schema for a data frame, listing all column names and types
#' as field names and (converted) types.
#'
#' See `vignette("table-schema")` to learn how this function implements the
#' Data Package standard.
#'
#' @param data A data frame.
#' @return List describing a Table Schema.
#' @family create functions
#' @export
#' @examples
#' # Create a data frame
#' df <- data.frame(
#'   id = c(as.integer(1), as.integer(2)),
#'   timestamp = c(
#'     as.POSIXct("2020-03-01 12:00:00", tz = "EET"),
#'     as.POSIXct("2020-03-01 18:45:00", tz = "EET")
#'   ),
#'   life_stage = factor(c("adult", "adult"), levels = c("adult", "juvenile"))
#' )
#'
#' # Create a Table Schema from the data frame
#' schema <- create_schema(df)
#' str(schema)
create_schema <- function(data) {
  # Check data
  check_data(data)

  # Columns with all NA are considered logical by R (and read_delim)
  # Set those to character, since string is a better default for Table Schema
  data_as_list <- lapply(data, function(x) {
    if (is.logical(x) && all(is.na(x))) {
      as.character(x)
    } else {
      x
    }
  })

  # Create fields (a list of lists)
  fields <- purrr::imap(data_as_list, function(x, name) {
    # Name
    name <- if (is.na(name)) "" else name

    # Type
    type <- paste(class(x), collapse = ",") # When data type is a vector
    type <- dplyr::recode(type,
      "character" = "string",
      "Date" = "date",
      "difftime" = "number",
      "factor" = "string",
      "hms,difftime" = "time", # Data read using col_time()
      "integer" = "integer",
      "logical" = "boolean",
      "numeric" = "number", # Includes double
      "POSIXct,POSIXt" = "datetime", # Includes POSIXlt,POSIXt
      .default = "any"
    )

    # Enumeration
    enum <- levels(x)

    # Create field list
    list(
      name = name,
      type = type,
      constraints = list(
        enum = enum
      )
    )
  })

  # Create schema
  schema <- list(
    fields = unname(fields) # Creates [] rather than {}
  )

  # Remove elements that are NULL or empty list
  schema <- clean_list(
    schema,
    function(x) is.null(x) || length(x) == 0L,
    recursive = TRUE
  )

  return(schema)
}
