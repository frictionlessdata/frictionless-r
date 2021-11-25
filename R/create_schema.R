#' Create a Table Schema for a data frame
#'
#' Creates a [Table Schema](https://specs.frictionlessdata.io/table-schema/) for
#' a data frame, listing all column names and types as field names and
#' (converted) types.
#'
#' @param df A data frame.
#'
#' @return List object describing the Table Schema.
#'
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr case_when recode %>%
#' @importFrom glue glue
#' @importFrom lubridate tz
#' @importFrom purrr imap
#'
#' @section Table schema properties:
#'
#' The Table Schema will be created from the data frame columns:
#'
#' - `name`: contains the column name.
#' - `type`: contains the converted column type (see further).
#' - `format`: not set and can thus be considered `default`. This is also the
#' case for dates, times and datetimes, since [readr::write_csv()] used by
#' [write_package()] will format those to ISO8601 which is considered the
#' default. Datetimes in local or non-UTC timezones will be converted to UTC
#' before writing.
#' - `description`: not set.
#' - `constraints`: not set, except for factors (see further).
#' - `missingValues`: not set. [write_package()] will use the default `""` for
#' missing values.
#' - `primaryKey`: not set.
#' - `foreignKeys`: not set.
#'
#' ## Field types
#'
#' The column type will determine the field `type`, as follows:
#'
#' - `character` →
#' [string](https://specs.frictionlessdata.io/table-schema/#string).
#' - `factor` →
#' [string](https://specs.frictionlessdata.io/table-schema/#string) with factor
#' levels as `enum`.
#' - `integer` →
#' [integer](https://specs.frictionlessdata.io/table-schema/#integer).
#' - `numeric` →
#' [number](https://specs.frictionlessdata.io/table-schema/#number).
#' - `logical` →.
#' [boolean](https://specs.frictionlessdata.io/table-schema/#boolean).
#' - `Date` → [date](https://specs.frictionlessdata.io/table-schema/#date).
#' - `POSIXct`/`POSIXlt` →
#' [datetime](https://specs.frictionlessdata.io/table-schema/#datetime).
#' - [hms::hms()] →
#' [time](https://specs.frictionlessdata.io/table-schema/#time).
#' - Any other type →
#' [any](https://specs.frictionlessdata.io/table-schema/#any).
#'
#' @examples
#' # Create data frame
#' df <- data.frame(
#'   id = c(as.integer(1), as.integer(2)),
#'   timestamp = c(as.POSIXct("2020-03-01 12:00:00", tz = "EET"), as.POSIXct("2020-03-01 18:45:00", tz = "EET")),
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
      # Name
      name <- ifelse(is.na(name), "", name)

      # Type
      type <- paste(class(x), collapse = ",") # When data type is a vector
      type <- recode(type,
        "character" = "string",
        "factor" = "string",
        "integer" = "integer",
        "numeric" = "number", # Includes double
        "logical" = "boolean",
        "Date" = "date",
        "POSIXct,POSIXt" = "datetime", # Includes POSIXlt,POSIXt
        "hms,difftime" = "time", # Data read using col_time()
        .default = "any"
      )

      # Enumeration
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
    fields = unname(fields) # Creates [] rather than {}
  )

  # TODO: Remove elements that are NULL

  schema
}
