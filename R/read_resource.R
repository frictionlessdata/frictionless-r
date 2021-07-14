#' Read data from a Tabular Data Resource into a tibble
#'
#' Reads data from a Data Package **resource** into a tibble (a Tidyverse data
#' frame). The resource has to meet the requirements of a [Tabular Data
#' Resource](https://specs.frictionlessdata.io/tabular-data-resource/). The
#' function is a wrapper around [readr::read_delim()], passing the resource
#' properties `path`, CSV dialect, column names, data types, etc. Column names
#' are taken from the provided `schema`, not from the header in the CSV file(s).
#'
#' @param package Package object, see `read_package()`.
#' @param resource_name Name of the resource to load data from.
#'
#' @return A [tibble()] with the resource data.
#'
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows desc pull tibble %>%
#' @importFrom glue glue
#' @importFrom httr http_error
#' @importFrom jsonlite fromJSON
#' @importFrom purrr keep map map_chr
#' @importFrom readr col_character col_date col_datetime col_double col_factor
#'   col_guess col_logical col_number col_time locale read_delim
#'
#' @details
#' The [`resource`](https://specs.frictionlessdata.io/data-resource/) properties
#' are handled as follows:
#'
#' ## Path
#'
#' [`path`](https://specs.frictionlessdata.io/data-resource/#data-location) is
#' required. It can be a local path or URL, which must resolve. Absolute path
#' (`/`) and relative parent path (`../`) are forbidden to avoid security
#' vulnerabilities.
#'
#' When multiple paths are provided (`"path": [ "myfile1.csv", "myfile2.csv"]`)
#' then data are merged into a single data frame, in the order in which the
#' paths are listed.
#'
#' ## Data
#'
#' Inline `data` is not supported.
#'
#' ## Name
#'
#' `name` is [required](https://specs.frictionlessdata.io/data-resource/#name).
#' It is used to find the resource with `name` = `resource_name`.
#'
#' ## Profile
#'
#' `profile` is
#' [required](https://specs.frictionlessdata.io/tabular-data-resource/#specification)
#' to have the value `tabular-data-resource`.
#'
#' ## File encoding
#'
#' `encoding` is required if the resource file(s) are not encoded as UTF-8. For
#' proper values (e.g. `windows-1252`), see "Preferred MIME Names" in
#' [encoding](https://specs.frictionlessdata.io/data-resource/#optional-properties).
#' The returned data frame will always be UTF-8.
#'
#' ## CSV Dialect
#'
#' `dialect` properties are
#' [required](https://specs.frictionlessdata.io/csv-dialect/#specification) if
#' the resource file(s) deviate from the default CSV settings (see below). Only
#' deviating properties need to be specified, e.g. a tab delimited file without
#' a header row needs:
#' ```json
#' "dialect": {"delimiter": "\t", "header": "false"}
#' ```
#'
#' These are the CSV dialect properties. Some are ignored by the function:
#' - `delimiter`: default `,`
#' - `lineTerminator`: ignored, line endings `LF`, `CRLF` and `CR` are
#' interpreted automatically by [readr::read_delim()].
#' - `quoteChar`: default `"`
#' - `doubleQuote`: default `true`
#' - `escapeChar`: anything but `\` is ignored and it will set `doubleQuote` to
#' `false` as these fields are mutually exclusive. You can thus not escape with
#' `\"` and `""` in the same file.
#' - `nullSequence`: ignored, use `missingValues`.
#' - `skipInitialSpace`: default `false`
#' - `header`: default `true`
#' - `commentChar`: not set by default.
#' - `caseSensitiveHeader`: ignored, header is not used for column names, see
#' Schema.
#' - `csvddfVersion`: ignored.
#'
#' ## Schema
#'
#' `schema` is required and must follow the [Table
#' Schema](http://specs.frictionlessdata.io/table-schema/) specification.
#'
#' - Field `name`s are used as column headers.
#' - [`missingValues`](https://specs.frictionlessdata.io/table-schema/#missing-values)
#' are used to interpret as `NA`, with `""` as default.
#'
#' ## Field types
#'
#' Field [`type`s](https://specs.frictionlessdata.io/table-schema/#types-and-formats)
#' are used a column types, as follows:
#'
#' - [`string`](https://specs.frictionlessdata.io/table-schema/#string) →
#' `character`; or `factor` when `enum` is present. `format` is ignored.
#' - [`number`](https://specs.frictionlessdata.io/table-schema/#number) →
#' `double`; or `factor` when `enum` is present. Use `bareNumber: false` to
#' ignore whitespace and non-numeric characters.
#' - [`integer`](https://specs.frictionlessdata.io/table-schema/#integer) →
#' `double` (not integer, to avoid issues with big numbers); or `factor` when
#' `enum` is present. Use `bareNumber: false` to ignore whitespace and
#' non-numeric characters.
#' - [`boolean`](https://specs.frictionlessdata.io/table-schema/#boolean) →
#' `logical`. Non-default `trueValues/falseValues` are not supported.
#' - [`object`](https://specs.frictionlessdata.io/table-schema/#object) →
#' `character`.
#' - [`array`](https://specs.frictionlessdata.io/table-schema/#array) →
#' `character`.
#' - [`date`]
#' - [`time`]
#' - [`datetime`]
#' - [`year`](https://specs.frictionlessdata.io/table-schema/#year) → `factor`.
#' - [`yearmonth`](https://specs.frictionlessdata.io/table-schema/#yearmonth) →
#' `factor`.
#' - [`duration`]
#' - [`geopoint`](https://specs.frictionlessdata.io/table-schema/#geopoint) →
#' `character`.
#' - [`geojson`](https://specs.frictionlessdata.io/table-schema/#geojson) →
#' `character`.
#' - [`any`](https://specs.frictionlessdata.io/table-schema/#any) → `character`.
#' - no type provided → type is guessed.
#' - unknown type → type is guessed.
#'
#' ## File compression
#'
#' Resource file(s) with `path` ending in `.gz`, `.bz2`, `.xz`, or `.zip` are
#' automatically decompressed using default [readr::read_delim()]
#' functionality. Only `.gz` files can be read directly from URL `path`s.
#' Only the extension in `path` can be used to indicate compression type,
#' the `compression` property is
#' [ignored](https://specs.frictionlessdata.io/patterns/#specification-3).
#'
#' ## Ignored resource properties
#'
#' - `title`
#' - `description`
#' - `format`
#' - `mediatype`
#' - `bytes`
#' - `hash`
#' - `sources`
#' - `licenses`
#'
#' @examples
#' # Read datapackage.json file
#' package <- read_package(system.file("extdata", "datapackage.json", package = "datapackage"))
#'
#' # List resource names
#' package$resource_names
#'
#' # Read data from resource "observations"
#' read_resource(package, "observations")
#'
#' # The above tibble is merged from 2 files listed in the resource path
#' package$resources[[2]]$path
#'
#' # With col_names and col_types derived from the resource schema
#' purrr::map_chr(package$resources[[2]]$schema$fields, "name")
#' purrr::map_chr(package$resources[[2]]$schema$fields, "type")
read_resource <- function(package, resource_name) {
  # Helper function to assign value when property is NULL
  replace_null <- function(value, replace) {
    if(!is.null(value)) { value } else { replace }
  }

  # Helper function to get unique values from vector sorted by occurrence
  unique_sorted <- function(x) {
    aggregate(x, by = list(x), FUN = length) %>%
      arrange(desc(x)) %>%
      pull(Group.1)
  }

  # Check package
  assert_that(
    class(package) == "list",
    msg = glue(
      "`package` must be a list object containing descriptor information,",
      "see read_package().", .sep = " "
    )
  )
  assert_that(
    !is.null(package$resource_names),
    msg = glue(
      "`package` must have property `resource_names`, load with read_package()."
    )
  )

  # Select resource
  resource_names_collapse <- paste(package$resource_names, collapse = ", ")
  assert_that(
    resource_name %in% package$resource_names,
    msg = glue(
      "Can't find resource `{resource_name}` in `{resource_names_collapse}`."
    )
  )
  resource <- keep(package$resources, function(x) {
    (x$name == resource_name)
  })[[1]]

  # Check if resource is `tabular-data-resource`
  assert_that(
    replace_null(resource$profile, "") == "tabular-data-resource",
    msg = glue(
      "Resource `{resource_name}` must have property `profile` with value",
      "`tabular-data-resource`.", .sep = " "
    )
  )

  # Select and verify path(s) to file(s)
  # https://specs.frictionlessdata.io/data-resource/#data-location
  assert_that(
    !is.null(resource$path),
    msg = glue("Resource `{resource_name}` must have property `path`.")
  )
  paths <-
    resource$path %>%
    # If not URL, append directory to create a full path
    map_chr(function(path) {
      if (startsWith(path, "http")) {
        path
      } else {
        assert_that(
          !startsWith(path, "/"),
          msg = glue(
            "{path} is an absolute path (`/`) which is forbidden to avoid",
            "security vulnerabilities.", .sep = " "
          )
        )
        assert_that(
          !startsWith(path, "../"),
          msg = glue(
            "{path} is a relative parent path (`../`) which is forbidden to",
            "avoid security vulnerabilities.", .sep = " "
          )
        )
        paste(package$directory, path, sep = "/")
      }
    })
  for (path in paths) {
    if (startsWith(path, "http")) {
      assert_that(
        !http_error(path),
        msg = glue("Can't find file at `{path}`.")
      )
    } else {
      assert_that(
        file.exists(path),
        msg = glue("Can't find file at `{path}`.")
      )
    }
  }

  # Select schema fields
  fields <- resource$schema$fields
  assert_that(
    !is.null(fields),
    msg = glue(
      "Resource `{resource_name}` must have property `schema` containing",
      "`fields`.", .sep = " "
    )
  )

  # Create locale with encoding, decimal_mark and grouping_mark
  d_chars <- map_chr(fields, ~ replace_null(.x$decimalChar, NA_character_))
  d_chars <- unique_sorted(d_chars)
  if (length(d_chars) == 0 | (length(d_chars) == 1 & d_chars[1] == ".")) {
    decimal_mark <- "." # Undefined or all set to default
  } else {
    decimal_mark <- d_chars[1]
    warning(glue(
      "Some fields define a non-default `decimalChar`. Only a global value is",
      "supported, so all number fields will be parsed with `{d_chars[1]}` as",
      "decimal mark.", .sep = " "
    ))
  }
  g_chars <- map_chr(fields, ~ replace_null(.x$groupChar, NA_character_))
  g_chars <- unique_sorted(g_chars)
  if (length(g_chars) == 0 | (length(g_chars) == 1 & g_chars[1] == "")) {
    grouping_mark <- "" # Undefined or all set to default
  } else {
    grouping_mark <- g_chars[1]
    warning(glue(
      "Some fields define a non-default `groupChar`. Only a global value is",
      "supported, so all number fields with this property will be parsed with",
      "`{g_chars[1]}` as grouping mark.", .sep = " "
    ))
  }
  locale <- locale(
    encoding = replace_null(resource$encoding, "UTF-8"),
    decimal_mark = decimal_mark,
    grouping_mark = grouping_mark
  )

  # Create col_names: c("name1", "name2", ...)
  col_names <- map_chr(fields, ~ replace_null(.x$name, NA_character_))
  assert_that(all(!is.na(col_names)),
    msg = glue(
      "Field {which(is.na(col_names))} of resource `{resource_name}` must",
      "have the property `name`.", .sep = " "
    )
  )

  # Create col_types: list(<collector_character>, <collector_logical>, ...)
  col_types <- map(fields, function(x) {
    type <- replace_null(x$type, NA_character_)
    enum <- x$constraints$enum
    group_char <- ifelse(replace_null(x$groupChar, "") != "", TRUE, FALSE)
    bare_number <- ifelse(replace_null(x$bareNumber, TRUE), TRUE, FALSE)
    format <- replace_null(x$format, "")
    format <- ifelse(format == "any", "", format) # Set "any" to ""

    col_type <- switch(type,
      "string" = if(length(enum) > 0) {
          col_factor(levels = enum)
        } else {
          col_character()
        },
      "number" = if(length(enum) > 0) {
          col_factor(levels = as.character(enum))
        } else if (group_char) {
          col_number() # Supports grouping_mark
        } else if (bare_number) {
          col_double() # Allows NaN, INF, -INF
        } else {
          col_number() # Strips non-numeric
        },
      "integer" = if(length(enum) > 0) {
          col_factor(levels = as.character(enum))
        } else if (bare_number) {
          col_double() # Not col_integer() to avoid issues with big integers
        } else {
          col_number() # Strips non-numeric
        },
      "boolean" = col_logical(),
      "object" = col_character(),
      "array" = col_character(),
      "date" = col_date(format = format),
      "time" = col_time(format = format),
      "datetime" = col_datetime(format = format),
      "year" = col_factor(),
      "yearmonth" = col_factor(),
      "duration" = col_guess(),
      "geopoint" = col_character(),
      "geojson" = col_character(),
      "any" = col_character()
    )
    # col_type will be NULL when type is undefined (NA_character) or an
    # unrecognized value (e.g. "datum"). Set those to col_guess()
    col_type <- replace_null(col_type, col_guess())
    col_type
  })
  # Assign names: list("name1" = <collector_character>, "name2" = ...)
  names(col_types) <- col_names

  # Select CSV dialect, see https://specs.frictionlessdata.io/csv-dialect/
  dialect <- resource$dialect # Can be NULL

  # Read data
  dataframes <- list()
  for (i in 1:length(paths)) {
    data <- read_delim(
      file = paths[i],
      delim = replace_null(dialect$delimiter, ","),
      quote = replace_null(dialect$quoteChar, "\""),
      escape_backslash = ifelse(
        replace_null(dialect$escapeChar, "not set") == "\\", TRUE, FALSE
      ),
      escape_double = ifelse(
        # if escapeChar is set, set doubleQuote to FALSE (mutually exclusive)
        replace_null(dialect$escapeChar, "not set") == "\\",
        FALSE,
        replace_null(dialect$doubleQuote, TRUE)
      ),
      col_names = col_names,
      col_types = col_types,
      locale = locale,
      na = replace_null(resource$schema$missingValues, ""),
      quoted_na = TRUE,
      comment = replace_null(dialect$commentChar, ""),
      trim_ws = replace_null(dialect$skipInitialSpace, FALSE),
      # Skip header row when present
      skip = ifelse(replace_null(dialect$header, TRUE), 1, 0),
      skip_empty_rows = TRUE
    )
    dataframes[[i]] <- data
  }

  # Merge data frames for all paths
  bind_rows(dataframes)
}
