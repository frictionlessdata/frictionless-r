#' Create a Table Schema for a data frame
#'
#' Creates a [Table Schema](https://specs.frictionlessdata.io/table-schema/) for
#' a data frame. The schema will contain all data frame fields, their `name` and
#' `type`.
#'
#' @param df A data frame.
#'
#' @return List object describing the Table Schema.
#'
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr recode %>%
#' @importFrom glue glue
#' @importFrom purrr imap
#'
#' @examples
#' # Create data frame
#' df <- data.frame(
#'   id = c(as.integer(1), as.integer(2)),
#'   timestamp = c(as.POSIXct("2020-03-01T12:00:00"), as.POSIXct("2020-03-01T18:45:00")),
#'   species = c("Capreolus capreolus", "Sus scrofa"),
#'   lifeStage = factor(c("adult", "adult"), levels = c("adult", "juvenile", "unknown"))
#' )
#'
#' # Create Table Schema
#' schema <- create_schema(df)
#' str(schema)
create_schema <- function(df) {
  # Check df
  assert_that(
    is.data.frame(df),
    msg = glue("`df` must be a data frame.")
  )

  # Create fields (a list of lists)
  fields <-
    df %>%
    imap(function(x, name) {
      name <- name
      type <- paste(class(x), collapse = ",") # When data type is a vector
      type <- recode(type,
        "logical" = "boolean",
        "integer" = "integer",
        "numeric" = "number",    # Includes double
        "character" = "string",
        "factor" = "string",     # !
        "Date" = "date",
        "hms,difftime" = "time",
        "POSIXct,POSIXt" = "datetime",
        .default = "any"
      )
      enum <- levels(x)

      # Create field list object
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
    fields = fields
  )

  # TODO: Remove elements that are NULL

  schema
}
