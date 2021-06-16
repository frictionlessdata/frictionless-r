#' Create a Table Schema for a data frame
#'
#' Creates a [Table Schema](https://specs.frictionlessdata.io/table-schema/) for
#' a data frame. `fields` will have properties `name` = column name and `type` =
#' translated column class. `missingValues` are set to `c("", "NA"`).
#'
#' @param df A data frame.
#'
#' @return List object with Table Schema properties.
#'
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr recode %>%
#' @importFrom glue glue
#' @importFrom purrr map_chr transpose
#'
#' @examples
#' # Create data frame
#' df <- data.frame(
#'   id = c(as.integer(1), as.integer(2)),
#'   timestamp = c(as.POSIXct("2020-03-01T12:00:00"), as.POSIXct("2020-03-01T18:45:00")),
#'   species = c("Capreolus capreolus", "Sus scrofa")
#' )
#'
#' # Create and print its schema
#' schema <- create_schema(df)
#' jsonlite::toJSON(schema, pretty = TRUE, auto_unbox = TRUE)
create_schema <- function(df) {
  # Check df
  assert_that(
    is.data.frame(df),
    msg = glue("`df` must be a data frame.")
  )

  # Get names
  name <- colnames(df)

  # Get types
  type <-
    df %>%
    map_chr(function(x) paste(class(x), collapse = ",")) %>%
    unname()
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
  fields <- data.frame(name, type)

  # create schema
  schema <- list(
    fields = transpose(fields),
    missingValues = c("", "NA")
  )

  schema
}
