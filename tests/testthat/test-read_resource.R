test_that("read_resource() returns a tibble", {
  pkg <- example_package
  resource_path <- read_resource(pkg, "deployments")
  resource_data <- read_resource(pkg, "media")
  pkg$resources[[3]]$data <- data.frame() # Media resource
  resource_df <- read_resource(pkg, "media")

  expect_s3_class(resource_path, "data.frame")
  expect_s3_class(resource_path, "tbl")
  expect_s3_class(resource_data, "data.frame")
  expect_s3_class(resource_data, "tbl")
  expect_s3_class(resource_df, "data.frame")
  expect_s3_class(resource_df, "tbl")
})

test_that("read_resource() returns error on incorrect Data Package", {
  expect_error(
    read_resource(list(), "deployments"),
    "`package` must be a list object of class `datapackage`"
  )
})

test_that("read_resource() returns error on incorrect Data Resource", {
  pkg <- example_package

  # No such resource
  expect_error(read_resource(pkg, "no_such_resource"), "Can't find resource")

  # Create invalid package and add properties one by one to pass errors
  pkg_invalid <- create_package()
  pkg_invalid$resources <- list(list(name = "deployments"))
  pkg_invalid$resource_names <- c("deployments")

  # No path or data
  expect_error(
    read_resource(pkg_invalid, "deployments"), "must have property `path` or `data`."
  )

  # No file at path url
  pkg_invalid$resources[[1]]$path <- "http://example.com/no_file.csv"
  expect_error(
    read_resource(pkg_invalid, "deployments"), "Can't find file at `http:"
  )

  # No file at path
  pkg_invalid$resources[[1]]$path <- "no_file.csv"
  expect_error(
    read_resource(pkg_invalid, "deployments"), "Can't find file at `./no_file.csv"
  )

  # No file at paths
  pkg_invalid$resources[[1]]$path <- c("deployments.csv", "no_file.csv")
  expect_error(read_resource(pkg_invalid, "deployments"), "Can't find file at")

  # Path is absolute path
  pkg_invalid$resources[[1]]$path <- "/inst/extdata/deployments.csv"
  expect_error(
    read_resource(pkg_invalid, "deployments"), "is an absolute path"
  )

  # Path is relative parent path
  pkg_invalid$resources[[1]]$path <- "../../inst/extdata/deployments.csv"
  expect_error(
    read_resource(pkg_invalid, "deployments"), "is a relative parent path"
  )

  # Add valid path
  pkg_invalid$resources[[1]]$path <- "deployments.csv"
  pkg_invalid$directory <- dirname(
    system.file("extdata", "datapackage.json", package = "frictionless")
  )

  # Not a tabular-data-resource
  expect_error(
    read_resource(pkg_invalid, "deployments"),
    "must have property `profile` with value `tabular-data-resource`"
  )

  # No schema
  pkg_invalid$resources[[1]]$profile <- "tabular-data-resource"
  expect_error(
    read_resource(pkg_invalid, "deployments"), "must have property `schema`."
  )

  # No file at schema url
  pkg_invalid$resources[[1]]$schema <- "http://example.com/no_schema.json"
  expect_error(read_resource(pkg_invalid, "deployments"), "Can't find file at")

  # No file at schema
  pkg_invalid$resources[[1]]$schema <- "no_schema.json"
  expect_error(read_resource(pkg_invalid, "deployments"), "Can't find file at")

  # Schema is absolute path
  pkg_invalid$resources[[1]]$schema <- "/tests/testthat/data/deployments_schema.json"
  expect_error(read_resource(pkg_invalid, "deployments"), "is an absolute path")

  # Schema is relative parent path
  pkg_invalid$resources[[1]]$schema <- "../testthat/data/deployments_schema.json"
  expect_error(read_resource(pkg_invalid, "deployments"), "is a relative parent path")

  # No fields
  pkg_invalid$resources[[1]]$schema <- list()
  expect_error(
    read_resource(pkg_invalid, "deployments"),
    "`schema` must be a list with property `fields`."
  )

  # No field name
  pkg_invalid$resources[[1]]$schema <- list(
    fields = list(
      list(name = "deployment_id"), # Field 1
      list(type = "number") # Field 2
    )
  )
  expect_error(
    read_resource(pkg_invalid, "deployments"),
    "All fields in `schema` must have property `name`."
  )
})

test_that("read_resource() can read newly added data (ignoring schema)", {
  pkg <- example_package
  df <- data.frame(
    "col_1" = c(1, 2),
    "col_2" = factor(c("a", "b"), levels = c("a", "b", "c"))
  )
  pkg$resources[[3]]$data <- df # Media resource
  expect_equal(read_resource(pkg, "media"), dplyr::as_tibble(df))
})

test_that("read_resource() can read inline data (ignoring schema)", {
  pkg <- example_package
  expected_resource <- readr::read_csv("data/media.csv", col_types = "ccccc")
  expect_equal(read_resource(pkg, "media"), expected_resource)

  pkg$resources[[3]]$data <- "not_a_list" # Media resource
  expect_error(read_resource(pkg, "media"))
})

test_that("read_resource() can read remote files", {
  pkg <- example_package
  resource <- read_resource(pkg, "deployments")

  pkg_remote_resource <- pkg
  pkg_remote_resource$resources[[1]]$path <- "https://github.com/frictionlessdata/frictionless-r/raw/main/inst/extdata/deployments.csv"
  expect_identical(resource, read_resource(pkg_remote_resource, "deployments"))
})

test_that("read_resource() can read local and remote Table Schemas", {
  pkg <- example_package
  resource <- read_resource(pkg, "deployments")

  pkg_local_schema <- pkg
  pkg_local_schema$directory <- "." # Use "./tests/testthat" outside test
  pkg_local_schema$resources[[1]]$schema <- "data/deployments_schema.json"
  # Using a remote path, otherwise schema and path need to share same directory
  pkg_local_schema$resources[[1]]$path <- "https://github.com/frictionlessdata/frictionless-r/raw/main/inst/extdata/deployments.csv"
  expect_identical(resource, read_resource(pkg_local_schema, "deployments"))

  pkg_remote_schema <- pkg
  pkg_remote_schema$resources[[1]]$schema <- "https://github.com/frictionlessdata/frictionless-r/raw/main/tests/testthat/data/deployments_schema.json"
  expect_identical(resource, read_resource(pkg_remote_schema, "deployments"))
})

test_that("read_resource() can read local and remote CSV dialect", {
  pkg <- example_package
  resource <- read_resource(pkg, "deployments")

  pkg_local_dialect <- pkg
  pkg_local_dialect$directory <- "." # Use "./tests/testthat/data" outside test
  pkg_local_dialect$resources[[1]]$dialect <- "data/dialect.json"
  # Using a remote path, otherwise schema and path need to share same directory
  pkg_local_dialect$resources[[1]]$path <- "https://github.com/frictionlessdata/frictionless-r/raw/main/inst/extdata/deployments.csv"
  expect_identical(resource, read_resource(pkg_local_dialect, "deployments"))

  pkg_remote_dialect <- pkg
  pkg_remote_dialect$resources[[1]]$dialect <- "https://github.com/frictionlessdata/frictionless-r/raw/main/tests/testthat/data/dialect.json"
  expect_identical(resource, read_resource(pkg_remote_dialect, "deployments"))
})

test_that("read_resource() understands CSV dialect", {
  pkg <- example_package
  resource <- read_resource(pkg, "deployments")

  # Create package with non-default dialect properties
  pkg_dialect <- pkg
  pkg_dialect$directory <- "." # Use "./tests/testthat" outside test
  pkg_dialect$resources[[1]]$path <- "data/deployments_dialect.csv"
  pkg_dialect$resources[[1]]$dialect <- list(
    delimiter = "/",
    # lineTerminator
    quoteChar = "'",         # Used to wrap dates which contain delimiter ":"
    doubleQuote = TRUE,      # Will get set to FALSE because of escapeChar
    escapeChar = "\\",       # Used to escape "/" in comments
    # nullSequence: not interpreted
    skipInitialSpace = TRUE, # Used to skip spaces in comment
    header = FALSE,          # There is no header
    commentChar = "#"        # Used to skip comments in first rows
    # caseSensitiveHeader: not interpreted
    # csvddfVersion: not interpreted
  )
  resource_dialect <- read_resource(pkg_dialect, "deployments")
  # One attribute of this df will be different: skip = 0 (since no header)
  # The default read_resource() sets this to: skip = 1
  # Since that is not a difference we want to test, we overwrite it
  attr(resource_dialect, "spec")$skip <- 1
  expect_identical(resource, resource_dialect)
})

test_that("read_resource() understands missing values", {
  pkg <- example_package
  resource <- read_resource(pkg, "deployments")

  # Create package with non-default missing values
  pkg_missing <- pkg
  pkg_missing$directory <- "." # Use "./tests/testthat" outside test
  pkg_missing$resources[[1]]$path <- "data/deployments_missingvalues.csv"
  pkg_missing$resources[[1]]$schema$missingValues <-
    append(pkg_missing$resources[[1]]$schema$missingValues, "ignore")
  expect_identical(resource, read_resource(pkg_missing, "deployments"))
})

test_that("read_resource() understands encoding", {
  pkg <- example_package
  resource <- read_resource(pkg, "deployments")

  # Create package with non-default missing values
  pkg_encoding <- pkg
  pkg_encoding$directory <- "." # Use "./tests/testthat" outside test
  pkg_encoding$resources[[1]]$path <- "data/deployments_encoding.csv"
  pkg_encoding$resources[[1]]$encoding <- "windows-1252"
  expect_identical(resource, read_resource(pkg_encoding, "deployments"))
})

test_that("read_resource() handles LF and CRLF line terminator characters", {
  # There are 3 line terminator characters:
  # LF    \n    Unix/Mac OS X
  # CR    \r    Max OS before X
  # CRLF  \r\n  Windows
  # According to spec, only LF and CRLF are allowed by default, otherwise the
  # dialect$lineTerminator should be used (with default CRLF)
  # https://specs.frictionlessdata.io/tabular-data-resource/#csv-file-requirements
  #
  # Line terminator characters can be checked in terminal with:
  #$ file deployments_crlf.csv
  #deployments_crlf.csv: UTF-8 Unicode text, with CRLF line terminators
  #
  # read_delim() however only handles 2 line terminator characters (LF and CRLF)
  # without explicitly indicating them, so dialect$lineTerminator is ignored
  pkg <- example_package
  resource <- read_resource(pkg, "deployments") # This file has LF

  pkg_crlf <- pkg
  pkg_crlf$directory <- "." # Use "./tests/testthat" outside test
  pkg_crlf$resources[[1]]$path <- "data/deployments_crlf.csv" # This file has CRLF
  expect_identical(resource, read_resource(pkg_crlf, "deployments"))
})

test_that("read_resource() can read compressed files", {
  pkg <- example_package
  resource <- read_resource(pkg, "deployments")

  # File created in terminal with:
  # zip deployments.csv.zip deployments.csv
  pkg_local_zip <- pkg
  pkg_local_zip$directory <- "." # Use "./tests/testthat" outside test
  pkg_local_zip$resources[[1]]$path <- "data/deployments.csv.zip"
  pkg_remote_zip <- pkg
  pkg_remote_zip$resources[[1]]$path <-
    "https://github.com/frictionlessdata/frictionless-r/raw/main/tests/testthat/data/deployments.csv.zip"

  # File created in terminal with:
  # gzip deployments.csv
  pkg_local_gz <- pkg
  pkg_local_gz$directory <- "." # Use "./tests/testthat" outside test
  pkg_local_gz$resources[[1]]$path <- "data/deployments.csv.gz"
  pkg_remote_gz <- pkg
  pkg_remote_gz$resources[[1]]$path <-
    "https://github.com/frictionlessdata/frictionless-r/raw/main/tests/testthat/data/deployments.csv.gz"

  expect_identical(resource, read_resource(pkg_local_zip, "deployments"))
  # Remote zip not supported, see
  # https://github.com/tidyverse/readr/issues/1042#issuecomment-545103047
  expect_error(read_resource(pkg_remote_zip, "deployments"))
  expect_identical(resource, read_resource(pkg_local_gz, "deployments"))
  expect_identical(resource, read_resource(pkg_remote_gz, "deployments"))
})

test_that("read_resource() handles strings", {
  pkg <- suppressMessages(read_package("data/types.json"))
  resource <- read_resource(pkg, "string")
  expect_type(resource$str, "character")

  # Use factor when enum is present
  enum <- pkg$resources[[1]]$schema$fields[[2]]$constraints$enum
  expect_s3_class(resource$str_factor, "factor")
  expect_identical(levels(resource$str_factor), enum)
})

test_that("read_resource() handles numbers", {
  pkg <- suppressMessages(read_package("data/types.json"))
  resource <- read_resource(pkg, "number")

  # Leading/trailing zeros are optional, + is assumed
  expect_type(resource$num, "double")
  expect_true(all(resource$num == 3))
  expect_type(resource$num_neg, "double")
  expect_true(all(resource$num_neg == -3))

  # Use factor when enum is present
  enum <- pkg$resources[[2]]$schema$fields[[3]]$constraints$enum
  expect_s3_class(resource$num_factor, "factor")
  expect_identical(levels(resource$num_factor), as.character(enum))

  # NaN, INF, -INF are supported, case-insensitive
  expect_type(resource$num_nan, "double")
  expect_true(all(is.nan(resource$num_nan)))
  expect_type(resource$num_inf, "double")
  expect_true(all(resource$num_inf == Inf))
  expect_type(resource$num_ninf, "double")
  expect_true(all(resource$num_ninf == -Inf))

  # Number can be expressed with E+-digits
  expect_type(resource$num_sci, "double")

  # bareNumber = false allows whitespace and non-numeric characters
  expect_type(resource$num_ws, "double")
  expect_true(all(resource$num_ws == 3.1))
  expect_type(resource$num_notbare, "double")
  expect_true(all(resource$num_notbare == 3.1))
})

test_that("read_resource() handles integers (as doubles)", {
  pkg <- suppressMessages(read_package("data/types.json"))
  resource <- read_resource(pkg, "integer")

  # Leading/trailing zeros are optional, + is assumed
  expect_type(resource$int, "double")
  expect_true(all(resource$int == 3))
  expect_type(resource$int_neg, "double")
  expect_true(all(resource$int_neg == -3))

  # Use factor when enum is present
  enum <- pkg$resources[[3]]$schema$fields[[3]]$constraints$enum
  expect_s3_class(resource$int_factor, "factor")
  expect_identical(levels(resource$int_factor), as.character(enum))

  # bareNumber = false allows whitespace and non-numeric characters
  expect_type(resource$int_ws, "double")
  expect_true(all(resource$int_ws == 3))
  expect_type(resource$int_notbare, "double")
  expect_true(all(resource$int_notbare == 3))
})

test_that("read_resource() handles booleans", {
  pkg <- suppressMessages(read_package("data/types.json"))
  resource <- read_resource(pkg, "boolean")

  # Default trueValues/falseValues are cast to logical
  expect_type(resource$bool_true, "logical")
  expect_type(resource$bool_false, "logical")
  expect_true(all(resource$bool_true == TRUE))
  expect_true(all(resource$bool_false == FALSE))
})

test_that("read_resource() handles dates", {
  expected_value <- as.Date("2013-11-23")
  pkg <- suppressMessages(read_package("data/types.json"))
  resource <- read_resource(pkg, "date")
  # This test covers:
  # - year: %Y %y
  # - month: %m (including 1 digit) %b %B
  # - day: %d (including 1 digit) %a not %A, see https://github.com/tidyverse/readr/issues/1230
  # - shortcut: %x (as %m/%d/%y, not the readr default %y/%m/%d)

  expect_identical(resource$dt_undefined, resource$dt_default)
  expect_identical(resource$dt_undefined, expected_value)
  expect_identical(resource$dt_default, expected_value)
  expect_identical(resource$dt_any, expected_value)
  expect_identical(resource$dt_shortcut, expected_value)
  expect_identical(resource$dt_1, expected_value)
  expect_identical(resource$dt_2, expected_value)
  expect_identical(resource$dt_3, expected_value)
})

test_that("read_resource() handles times", {
  expected_value <- hms::hms(0, 30, 8) # "08:30:00"
  pkg <- suppressMessages(read_package("data/types.json"))
  resource <- read_resource(pkg, "time")
  # This test covers:
  # - hour: %H (including 1 digit) %I + %p
  # - minute: %M
  # - seconds: %S
  # - milli/microseconds: %f
  # - timezone: %Z %z
  # - shortcut: %X (as %H:%M:%S)

  expect_identical(resource$tm_undefined, resource$tm_default)
  expect_identical(resource$tm_undefined, expected_value)
  expect_identical(resource$tm_default, expected_value)
  expect_identical(resource$tm_any, expected_value)
  expect_identical(resource$tm_shortcut, expected_value)
  expect_identical(resource$tm_1, expected_value)
  expect_identical(resource$tm_2, expected_value)
  expect_identical(resource$tm_3, hms::hms(0.3, 30, 8)) # "08:30:00.3"
  # col_time will correctly parse timezone offset (+0000) in tm_3, but not
  # convert to UTC, so 06:30:00.300-0200 = 06:30:00.3, not 08:30:00.3
})

test_that("read_resource() handles datetimes", {
  expected_value <- as.POSIXct("2013-11-23 08:30:00", tz = "UTC")
  pkg <- suppressMessages(read_package("data/types.json"))
  resource <- read_resource(pkg, "datetime")

  expect_identical(resource$dttm_undefined, resource$dttm_default)
  expect_identical(resource$dttm_undefined, expected_value)
  expect_identical(resource$dttm_default, expected_value)
  expect_identical(resource$dttm_any, expected_value)
  expect_identical(resource$dttm_1, expected_value)
  expect_identical(
    resource$dttm_2,
    as.POSIXct("2013-11-23 08:30:00.3", tz = "UTC")
  )
})

test_that("read_resource() handles other types", {
  pkg <- suppressMessages(read_package("data/types.json"))
  resource <- read_resource(pkg, "other")

  # Interpret year, yearmonth as dates
  expect_s3_class(resource$year, "Date")
  expect_identical(resource$year[1], as.Date("2001-01-01"))
  expect_s3_class(resource$yearmonth, "Date")
  expect_identical(resource$yearmonth[1], as.Date("2001-03-01"))

  # Interpret object, array, duration, geopoint, geojson as character
  expect_type(resource$object, "character")
  expect_type(resource$array, "character")
  expect_type(resource$duration, "character")
  expect_type(resource$geopoint, "character")
  expect_type(resource$geojson, "character")

  # Interpret any as character
  expect_type(resource$any, "character")

  # Guess undefined types, unknown types are blocked by check_schema()
  expect_type(resource$no_type, "logical")
})

test_that("read_resource() handles decimalChar/groupChar properties", {
  expected_value <- 3000000.3
  pkg <- suppressMessages(read_package("data/mark.json"))

  # Default decimalChar/groupChar
  resource <- read_resource(pkg, "mark_default")
  expect_identical(resource$num, expected_value) # 3000000.30
  expect_identical(resource$num_undefined, expected_value) # 3000000.30

  # Non-default decimalChar, default groupChar (which should not conflict)
  warnings <- capture_warnings(read_resource(pkg, "mark_decimal"))
  expect_match(warnings[1], "Some fields define a non-default `decimalChar`.")

  resource <- suppressWarnings(read_resource(pkg, "mark_decimal"))
  expect_identical(resource$num, expected_value) # 3000000.30
  expect_identical(resource$num_undefined, expected_value) # 3000000.30

  # Non-default decimalChar/groupChar
  warnings <- capture_warnings(read_resource(pkg, "mark_decimal_group"))
  expect_true(length(warnings) == 3) # 2 warnings + 1 parsing failure last field
  expect_match(warnings[1], "Some fields define a non-default `decimalChar`.")
  expect_match(warnings[2], "Some fields define a non-default `groupChar`.")

  resource <- suppressWarnings(read_resource(pkg, "mark_decimal_group"))
  expect_identical(resource$num, expected_value) # 3.000.000,30
  # Field without decimalChar is still parsed with non-default decimalChar
  expect_identical(resource$num_undefined, expected_value) # 3000000,30
  # Field without groupChar is not parsed with non-default groupChar
  expect_identical(resource$num_undefined_group, NA_real_) # 3.000.000,30
})
