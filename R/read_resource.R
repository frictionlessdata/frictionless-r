#' Read data from a Data Resource into a tibble data frame
#'
#' Reads data from a [Data
#' Resource](https://specs.frictionlessdata.io/data-resource/) (in a Data
#' Package) into a tibble (a Tidyverse data frame).
#' The resource must be a [Tabular Data
#' Resource](https://specs.frictionlessdata.io/tabular-data-resource/).
#' The function uses [readr::read_delim()] to read CSV files, passing the
#' resource properties `path`, CSV dialect, column names, data types, etc.
#' Column names are taken from the provided Table Schema (`schema`), not from
#' the header in the CSV file(s).
#'
#' @param package Data Package object, created with [read_package()] or
#'   [create_package()].
#' @param resource_name Name of the Data Resource.
#' @param col_select Character vector of the columns to include in the result,
#'   in the order provided.
#'   Selecting columns can improve read speed.
#' @return [tibble::tibble()] data frame with the Data Resource's tabular data.
#'   If there are parsing problems, a warning will alert you.
#'   You can retrieve the full details by calling [problems()] on your data
#'   frame.
#' @family read functions
#' @export
#' @section Resource properties:
#' The [Data Resource
#' properties](https://specs.frictionlessdata.io/data-resource/) are handled as
#' follows:
#'
#' ## Path
#'
#' [`path`](https://specs.frictionlessdata.io/data-resource/#data-location) is
#' required.
#' It can be a local path or URL, which must resolve.
#' Absolute path (`/`) and relative parent path (`../`) are forbidden to avoid
#' security vulnerabilities.
#'
#' When multiple paths are provided (`"path": [ "myfile1.csv", "myfile2.csv"]`)
#' then data are merged into a single data frame, in the order in which the
#' paths are listed.
#'
#' ## Data
#'
#' If `path` is not present, the function will attempt to read data from the
#' `data` property.
#' **`schema` will be ignored**.
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
#' `encoding` (e.g. `windows-1252`) is
#' [required](https://specs.frictionlessdata.io/data-resource/#optional-properties)
#' if the resource file(s) is not encoded as UTF-8.
#' The returned data frame will always be UTF-8.
#'
#' ## CSV Dialect
#'
#' `dialect` properties are
#' [required](https://specs.frictionlessdata.io/csv-dialect/#specification) if
#' the resource file(s) deviate from the default CSV settings (see below).
#' It can either be a JSON object or a path or URL referencing a JSON object.
#' Only deviating properties need to be specified, e.g. a tab delimited file
#' without a header row needs:
#' ```json
#' "dialect": {"delimiter": "\t", "header": "false"}
#' ```
#'
#' These are the CSV dialect properties.
#' Some are ignored by the function:
#' - `delimiter`: default `,`.
#' - `lineTerminator`: ignored, line terminator characters `LF` and `CRLF` are
#'   interpreted automatically by [readr::read_delim()], while `CR` (used by
#'   Classic Mac OS, final release 2001) is not supported.
#' - `doubleQuote`: default `true`.
#' - `quoteChar`: default `"`.
#' - `escapeChar`: anything but `\` is ignored and it will set `doubleQuote` to
#'   `false` as these fields are mutually exclusive.
#'   You can thus not escape with `\"` and `""` in the same file.
#' - `nullSequence`: ignored, use `missingValues`.
#' - `skipInitialSpace`: default `false`.
#' - `header`: default `true`.
#' - `commentChar`: not set by default.
#' - `caseSensitiveHeader`: ignored, header is not used for column names, see
#'   Schema.
#' - `csvddfVersion`: ignored.
#'
#' ## File compression
#'
#' Resource file(s) with `path` ending in `.gz`, `.bz2`, `.xz`, or `.zip` are
#' automatically decompressed using default [readr::read_delim()]
#' functionality.
#' Only `.gz` files can be read directly from URL `path`s.
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
#' @section Table schema properties:
#' `schema` is required and must follow the [Table
#' Schema](https://specs.frictionlessdata.io/table-schema/) specification.
#' It can either be a JSON object or a path or URL referencing a JSON object.
#'
#' - Field `name`s are used as column headers.
#' - Field `type`s are use as column types (see further).
#' - [`missingValues`](https://specs.frictionlessdata.io/table-schema/#missing-values)
#'   are used to interpret as `NA`, with `""` as default.
#'
#' ## Field types
#'
#' Field `type` is used to set the column type, as follows:
#'
#' - [string](https://specs.frictionlessdata.io/table-schema/#string) as
#'   `character`; or `factor` when `enum` is present.
#'   `format` is ignored.
#' - [number](https://specs.frictionlessdata.io/table-schema/#number) as
#'   `double`; or `factor` when `enum` is present.
#'   Use `bareNumber: false` to ignore whitespace and non-numeric characters.
#'   `decimalChar` (`.` by default) and `groupChar` (undefined by default) can
#'   be defined, but the most occurring value will be used as a global value for
#'   all number fields of that resource.
#' - [integer](https://specs.frictionlessdata.io/table-schema/#integer) as
#'   `double` (not integer, to avoid issues with big numbers); or `factor` when
#'   `enum` is present.
#'   Use `bareNumber: false` to ignore whitespace and non-numeric characters.
#' - [boolean](https://specs.frictionlessdata.io/table-schema/#boolean) as
#'   `logical`.
#'   Non-default `trueValues/falseValues` are not supported.
#' - [object](https://specs.frictionlessdata.io/table-schema/#object) as
#'   `character`.
#' - [array](https://specs.frictionlessdata.io/table-schema/#array) as
#'   `character`.
#' - [date](https://specs.frictionlessdata.io/table-schema/#date) as `date`.
#'   Supports `format`, with values `default` (ISO date), `any` (guess `ymd`)
#'   and [Python/C strptime](https://docs.python.org/2/library/datetime.html#strftime-strptime-behavior)
#'   patterns, such as `%a, %d %B %Y` for `Sat, 23 November 2013`.
#'   `%x` is `%m/%d/%y`.
#'   `%j`, `%U`, `%w` and `%W` are not supported.
#' - [time](https://specs.frictionlessdata.io/table-schema/#time) as
#'   [hms::hms()].
#'   Supports `format`, with values `default` (ISO time), `any` (guess `hms`)
#'   and [Python/C strptime](https://docs.python.org/2/library/datetime.html#strftime-strptime-behavior)
#'   patterns, such as `%I%p%M:%S.%f%z` for `8AM30:00.300+0200`.
#' - [datetime](https://specs.frictionlessdata.io/table-schema/#datetime) as
#'   `POSIXct`.
#'   Supports `format`, with values `default` (ISO datetime), `any`
#'   (ISO datetime) and the same patterns as for `date` and `time`.
#'   `%c` is not supported.
#' - [year](https://specs.frictionlessdata.io/table-schema/#year) as `date`,
#'   with `01` for month and day.
#' - [yearmonth](https://specs.frictionlessdata.io/table-schema/#yearmonth) as
#'   `date`, with `01` for day.
#' - [duration](https://specs.frictionlessdata.io/table-schema/#duration) as
#'   `character`.
#'   Can be parsed afterwards with [lubridate::duration()].
#' - [geopoint](https://specs.frictionlessdata.io/table-schema/#geopoint) as
#'   `character`.
#' - [geojson](https://specs.frictionlessdata.io/table-schema/#geojson) as
#'   `character`.
#' - [any](https://specs.frictionlessdata.io/table-schema/#any) as `character`.
#' - Any other value is not allowed.
#' - Type is guessed if not provided.
#' @examples
#' # Read a datapackage.json file
#' package <- read_package(
#'   system.file("extdata", "datapackage.json", package = "frictionless")
#' )
#'
#' package
#'
#' # Read data from the resource "observations"
#' read_resource(package, "observations")
#'
#' # The above tibble is merged from 2 files listed in the resource path
#' package$resources[[2]]$path
#'
#' # The column names and types are derived from the resource schema
#' purrr::map_chr(package$resources[[2]]$schema$fields, "name")
#' purrr::map_chr(package$resources[[2]]$schema$fields, "type")
#'
#' # Read data from the resource "deployments" with column selection
#' read_resource(package, "deployments", col_select = c("latitude", "longitude"))
read_resource <- function(package, resource_name, col_select = NULL) {
  # Get resource, includes check_package()
  resource <- get_resource(package, resource_name)

  # Get paths, schema and fields
  paths <- resource$path
  schema <- get_schema(package, resource_name)

  # From https://github.com/frictionlessdata/frictionless-r/pull/237/files
  # To be replaced with get_fields_names().

  fields <- purrr::chuck(schema, "fields")
  field_names <- purrr::map_chr(fields, ~ purrr::pluck(.x, "name"))

  # Check all selected columns appear in schema
  if (!all(col_select %in% field_names)) {
    col_select_missing <- col_select[!col_select %in% field_names]
    cli::cli_abort(
      c(
        "Can't find column{?s} {.val {col_select_missing}} in field names.",
        "i" = "Field name{?s}: {.val {field_names}}."
      ),
      class = "frictionless_error_colselect_mismatch"
    )
  }

  # Create locale with encoding, decimal_mark and grouping_mark
  locale <- create_locale(package, resource_name)

  # Create col_types: list(<collector_character>, <collector_logical>, ...)
  col_types <- create_col_types(package, resource_name)

  # Select CSV dialect, see https://specs.frictionlessdata.io/csv-dialect/
  # Note that dialect can be NULL
  dialect <- read_descriptor(resource$dialect, package$directory, safe = TRUE)

  # Read data directly
  if (resource$read_from == "df") {
    df <- dplyr::as_tibble(resource$data)

  # Read data from data
  } else if (resource$read_from == "data") {
    df <- dplyr::as_tibble(do.call(rbind.data.frame, resource$data))

  # Read data from path(s)
  } else if (resource$read_from == "path" || resource$read_from == "url") {
    df <- purrr::map_df(paths, read_from_path,
                        dialect = dialect,
                        field_names = field_names,
                        col_types = col_types,
                        col_select = col_select,
                        schema = schema,
                        locale = locale)
  }
  return(df)
}
