test_that("read_resource() returns a tibble", {
  p <- example_package()
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  p <- add_resource(p, "new", df)

  expect_s3_class(read_resource(p, "deployments"), "tbl") # via path
  expect_s3_class(read_resource(p, "media"), "tbl")       # via data
  expect_s3_class(read_resource(p, "new"), "tbl")         # via df
})

test_that("read_resource() allows column selection", {
  p <- example_package()

  # Single column
  expect_named(
    read_resource(p, "deployments", col_select = "start"),
    "start"
  )
  expect_identical(
    read_resource(p, "deployments", col_select = "start"),
    dplyr::select(
      read_resource(p, "deployments"),
      "start"
    ),
    ignore_attr = "spec" # read_delim() returns vroom::spec(), select() does not
  )

  # Multiple columns
  expect_named(
    read_resource(p, "deployments", col_select = c("deployment_id", "start")),
    c("deployment_id", "start")
  )
  expect_identical(
    read_resource(p, "deployments", col_select = c("deployment_id", "start")),
    dplyr::select(
      read_resource(p, "deployments"),
      "deployment_id",
      "start"
    ),
    ignore_attr = "spec"
  )

  # Different order
  expect_named(
    read_resource(
      p,
      "deployments",
      col_select = c("start", "deployment_id", "comments")
    ),
    c("start", "deployment_id", "comments"),
    ignore.order = FALSE
  )
  expect_identical(
    read_resource(
      p,
      "deployments",
      col_select = c("start", "deployment_id", "comments")
    ),
    dplyr::select(
      read_resource(p, "deployments"),
      "start",
      "deployment_id",
      "comments"
    ),
    ignore_attr = "spec"
  )
})

test_that("read_resource() returns error on column selection not in schema", {
  p <- example_package()

  # One column
  expect_error(
    read_resource(p, "deployments", col_select = "no_such_column"),
    class = "frictionless_error_colselect_mismatch"
  )
  expect_error(
    read_resource(p, "deployments", col_select = "no_such_column"),
    regexp = "Can't find column \"no_such_column\" in field names.",
    fixed = TRUE
  )
  expect_error(
    read_resource(p, "deployments", col_select = "no_such_column"),
    regexp = paste(
      "Field names: \"deployment_id\", \"longitude\", \"latitude\", \"start\",",
      "and \"comments\"."
    ),
    fixed = TRUE
  )

  # Partial match, multiple columns
  expect_error(
    read_resource(
      p,
      "deployments",
      col_select = c("no_such_column", "start", "no_such_column_either")
    ),
    class = "frictionless_error_colselect_mismatch"
  )
  expect_error(
    read_resource(
      p,
      "deployments",
      col_select = c("no_such_column", "start", "no_such_column_either")
    ),
    regexp = paste(
      "Can't find columns \"no_such_column\" and \"no_such_column_either\" in",
      "field names."
    ),
    fixed = TRUE
  )
})

test_that("read_resource() returns error on invalid Data Package", {
  expect_error(
    read_resource(list(), "deployments"),
    class = "frictionless_error_package_invalid"
  )
})

test_that("read_resource() returns error on invalid resource", {
  skip_if_offline()
  p <- example_package()

  # No such resource
  expect_error(
    read_resource(p, "no_such_resource"),
    class = "frictionless_error_resource_not_found"
  )

  # Create invalid package and add properties one by one to pass errors
  p_invalid <- create_package()
  p_invalid$resources <- list(list(name = "deployments"))

  # No path or data
  expect_error(
    read_resource(p_invalid, "deployments"),
    class = "frictionless_error_resource_without_path_data"
  )
  expect_error(
    read_resource(p_invalid, "deployments"),
    regexp = "Resource \"deployments\" must have a path or data property.",
    fixed = TRUE
  )

  # Both path or data
  p_invalid$resources[[1]]$path <- "value"
  p_invalid$resources[[1]]$data <- "value"
  expect_error(
    read_resource(p_invalid, "deployments"),
    class = "frictionless_error_resource_both_path_data"
  )
  expect_error(
    read_resource(p_invalid, "deployments"),
    regexp = paste(
      "Resource \"deployments\" must have a path or data property,",
      "not both."
    ),
    fixed = TRUE
  )

  # No file at path url
  p_invalid$resources[[1]]$data <- NULL
  p_invalid$resources[[1]]$path <- "https://example.com/no_such_file.csv"
  expect_error(
    read_resource(p_invalid, "deployments"),
    class = "frictionless_error_url_not_found"
  )

  # No file at path
  p_invalid$resources[[1]]$path <- "no_such_file.csv"
  expect_error(
    read_resource(p_invalid, "deployments"),
    class = "frictionless_error_path_not_found"
  )

  # No file at paths
  p_invalid$resources[[1]]$path <- c("deployments.csv", "no_such_file.csv")
  expect_error(
    read_resource(p_invalid, "deployments"),
    class = "frictionless_error_path_not_found"
  )

  # Path is absolute path
  p_invalid$resources[[1]]$path <- "/inst/extdata/deployments.csv"
  expect_error(
    read_resource(p_invalid, "deployments"),
    class = "frictionless_error_path_unsafe_absolute"
  )

  # Path is relative parent path
  p_invalid$resources[[1]]$path <- "../../inst/extdata/deployments.csv"
  expect_error(
    read_resource(p_invalid, "deployments"),
    class = "frictionless_error_path_unsafe_relative"
  )

  # Add valid path
  p_invalid$resources[[1]]$path <- "deployments.csv"
  p_invalid$directory <- dirname(
    system.file("extdata", "v1", "datapackage.json", package = "frictionless")
  )

  # Not a tabular-data-resource
  expect_error(
    read_resource(p_invalid, "deployments"),
    class = "frictionless_error_resource_not_tabular"
  )
  expect_error(
    read_resource(p_invalid, "deployments"),
    regexp = paste(
      "Resource \"deployments\" must have a profile property with value",
      "\"tabular-data-resource\"."
    ),
    fixed = TRUE
  )

  # No schema
  p_invalid$resources[[1]]$profile <- "tabular-data-resource"
  expect_error(
    read_resource(p_invalid, "deployments"),
    class = "frictionless_error_resource_without_schema"
  )
  expect_error(
    read_resource(p_invalid, "deployments"),
    regexp = "Resource \"deployments\" must have a schema property.",
    fixed = TRUE
  )

  # No file at schema url
  p_invalid$resources[[1]]$schema <- "https://example.com/no_schema.json"
  expect_error(
    read_resource(p_invalid, "deployments"),
    class = "frictionless_error_url_not_found"
  )

  # No file at schema
  p_invalid$resources[[1]]$schema <- "no_schema.json"
  expect_error(
    read_resource(p_invalid, "deployments"),
    class = "frictionless_error_path_not_found"
  )

  # No fields
  p_invalid$resources[[1]]$schema <- list()
  expect_error(
    read_resource(p_invalid, "deployments"),
    class = "frictionless_error_schema_invalid"
  )

  # No field name
  p_invalid$resources[[1]]$schema <- list(fields = list(
      list(name = "deployment_id"), # Field 1
      list(type = "number") # Field 2
    )
  )
  expect_error(
    read_resource(p_invalid, "deployments"),
    class = "frictionless_error_fields_without_name"
  )
})

test_that("read_resource() can read newly added data (ignoring schema)", {
  p <- example_package()
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  p <- add_resource(p, "new", df)
  expect_identical(read_resource(p, "new"), dplyr::as_tibble(df))
})

test_that("read_resource() can read inline data (ignoring schema)", {
  p <- example_package()
  expected_resource <- readr::read_csv(
    test_path("data/media.csv"),
    col_types = "ccccc"
  )
  expect_identical(read_resource(p, "media"), expected_resource)

  p$resources[[3]]$data <- "not_a_list"
  expect_error(
    read_resource(p, "media"),
    regexp = "second argument must be a list",
    fixed = TRUE
  )
})

test_that("read_resource() can read local files", {
  p <- example_package()
  resource <- read_resource(p, "deployments")

  p_local <- read_package(
    system.file("extdata", "v1", "datapackage.json", package = "frictionless")
  )
  expect_identical(read_resource(p_local, "deployments"), resource)
})

test_that("read_resource() can read remote files", {
  skip_if_offline()
  p <- example_package()
  resource <- read_resource(p, "deployments")

  p_remote_resource <- p
  p_remote_resource$resources[[1]]$path <- file.path(
    "https://raw.githubusercontent.com/frictionlessdata/frictionless-r",
    "main/inst/extdata/v1/deployments.csv"
  )
  expect_identical(read_resource(p_remote_resource, "deployments"), resource)
})

test_that("read_resource() can read safe local and remote Table Schema,
           including YAML", {
  skip_if_offline()
  p <- example_package()
  resource <- read_resource(p, "deployments")
  p$directory <- "."

  # Use a remote path, otherwise schema and path need to share same directory
  p$resources[[1]]$path <- file.path(
    "https://raw.githubusercontent.com/frictionlessdata/frictionless-r",
    "main/inst/extdata/v1/deployments.csv"
  )

  # Schema is absolute path
  p_unsafe <- p
  p_unsafe$resources[[1]]$schema <-
    "/tests/testthat/data/deployments_schema.json"
  expect_error(
    read_resource(p_unsafe, "deployments"),
    class = "frictionless_error_path_unsafe_absolute"
  )

  # Schema is relative parent path
  p_unsafe$resources[[1]]$schema <- "../testthat/data/deployments_schema.json"
  expect_error(
    read_resource(p_unsafe, "deployments"),
    class = "frictionless_error_path_unsafe_relative"
  )

  # Schema is local path
  p_local_schema <- p
  p_local_schema$resources[[1]]$schema <-
    test_path("data/deployments_schema.json")
  expect_identical(read_resource(p_local_schema, "deployments"), resource)

  # Schema is remote path
  p_remote_schema <- p
  p_remote_schema$resources[[1]]$schema <- file.path(
    "https://raw.githubusercontent.com/frictionlessdata/frictionless-r",
    "main/tests/testthat/data/deployments_schema.json"
  )
  expect_identical(read_resource(p_remote_schema, "deployments"), resource)

  # Schema is YAML
  p_yaml_schema <- p
  p_yaml_schema$resource[[1]]$schema <- test_path("data/deployment_schema.yaml")
  expect_identical(read_resource(p_yaml_schema, "deployments"), resource)
})

test_that("read_resource() can read safe local and remote CSV dialect", {
  skip_if_offline()
  p <- example_package()
  resource <- read_resource(p, "deployments")
  p$directory <- "."

  # Use a remote path, otherwise dialect and path need to share same directory
  p$resources[[1]]$path <- file.path(
    "https://raw.githubusercontent.com/frictionlessdata/frictionless-r",
    "main/inst/extdata/v1/deployments.csv"
  )

  # Dialect is absolute path
  p_unsafe <- p
  p_unsafe$resources[[1]]$dialect <- "/tests/testthat/data/dialect.json"
  expect_error(
    read_resource(p_unsafe, "deployments"),
    class = "frictionless_error_path_unsafe_absolute"
  )

  # Dialect is relative parent path
  p_unsafe$resources[[1]]$dialect <- "../testthat/data/dialect.json"
  expect_error(
    read_resource(p_unsafe, "deployments"),
    class = "frictionless_error_path_unsafe_relative"
  )

  # Dialect is local path
  p_local_dialect <- p
  p_local_dialect$resources[[1]]$dialect <- test_path("data/dialect.json")
  expect_identical(read_resource(p_local_dialect, "deployments"), resource)

  # Dialect is remote path
  p_remote_dialect <- p
  p_remote_dialect$resources[[1]]$dialect <- file.path(
    "https://raw.githubusercontent.com/frictionlessdata/frictionless-r",
    "main/tests/testthat/data/dialect.json"
  )
  expect_identical(read_resource(p_remote_dialect, "deployments"), resource)

  # Dialect is YAML
  p_yaml_dialect <- p
  p_yaml_dialect$resource[[1]]$dialect <- test_path("data/dialect.yaml")
  expect_identical(read_resource(p_yaml_dialect, "deployments"), resource)
})

test_that("read_resource() understands CSV dialect", {
  p <- example_package()
  resource <- read_resource(p, "deployments")

  # Create package with non-default dialect properties
  p_dialect <- p
  p_dialect$directory <- "."
  p_dialect$resources[[1]]$path <- test_path("data/deployments_dialect.csv")
  p_dialect$resources[[1]]$dialect <- list(
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
  resource_dialect <- read_resource(p_dialect, "deployments")
  # One attribute of this df will be different: skip = 0 (since no header)
  # The default read_resource() sets this to: skip = 1
  # Since that is not a difference we want to test, we overwrite it
  attr(resource_dialect, "spec")$skip <- 1
  expect_identical(resource_dialect, resource)
})

test_that("read_resource() understands missing values", {
  p <- example_package()
  resource <- read_resource(p, "deployments")

  # Create package with non-default missing values
  p_missing <- p
  p_missing$directory <- "."
  p_missing$resources[[1]]$path <-
    test_path("data/deployments_missingvalues.csv")
  p_missing$resources[[1]]$schema$missingValues <-
    append(p_missing$resources[[1]]$schema$missingValues, "ignore")
  expect_identical(read_resource(p_missing, "deployments"), resource)
})

test_that("read_resource() understands encoding", {
  p <- example_package()
  resource <- read_resource(p, "deployments")

  # Create package with non-default encoding
  p_encoding <- p
  p_encoding$directory <- "."
  p_encoding$resources[[1]]$path <- test_path("data/deployments_encoding.csv")
  p_encoding$resources[[1]]$encoding <- "windows-1252"
  expect_identical(read_resource(p_encoding, "deployments"), resource)

  # Create package with unknown encoding
  p_unknown <- p
  p_unknown$resources[[1]]$encoding <- "utf-8-sig"
  expect_warning(
    read_resource(p_unknown, "deployments"),
    class = "frictionless_warning_resource_encoding_unknown"
  )
  expect_warning(
    read_resource(p_unknown, "deployments"),
    regexp = "Unknown encoding utf-8-sig. Reading file(s) with UTF-8 encoding.",
    fixed = TRUE
  )
  expect_identical(
    suppressWarnings(read_resource(p_unknown, "deployments")),
    resource
  )
})

test_that("read_resource() handles decimalChar/groupChar properties", {
  expected_value <- 3000000.3
  p <- read_package(test_path("data/mark.json"))

  # Default decimalChar/groupChar
  resource <- read_resource(p, "mark_default")
  expect_identical(resource$num, expected_value) # 3000000.30
  expect_identical(resource$num_undefined, expected_value) # 3000000.30

  # Non-default decimalChar, default groupChar (which should not conflict)
  expect_warning(
    read_resource(p, "mark_decimal"),
    class = "frictionless_warning_fields_decimalchar_different"
  )
  expect_warning(
    read_resource(p, "mark_decimal"),
    regexp = paste(
      "Some fields define a non-default decimalChar.",
      "Parsing all number fields with \",\" as decimal mark."
    ),
    fixed = TRUE
  )

  resource <- suppressWarnings(read_resource(p, "mark_decimal"))
  expect_identical(resource$num, expected_value) # 3000000.30
  expect_identical(resource$num_undefined, expected_value) # 3000000.30

  # Non-default decimalChar and groupChar
  # Results in 3 warnings: decimalchar, groupchar, parsing failure last field
  suppressWarnings(expect_warning(
    read_resource(p, "mark_decimal_group"),
    class = "frictionless_warning_fields_decimalchar_different"
  ))
  suppressWarnings(expect_warning(
    read_resource(p, "mark_decimal_group"),
    class = "frictionless_warning_fields_groupchar_different"
  ))
  suppressWarnings(expect_warning(
    read_resource(p, "mark_decimal_group"),
    regexp = paste(
      "Some fields define a non-default groupChar.",
      "Parsing all number fields with \".\" as grouping mark."
    ),
    fixed = TRUE
  ))
  resource <- suppressWarnings(read_resource(p, "mark_decimal_group"))
  expect_identical(resource$num, expected_value) # 3.000.000,30
  # Field without decimalChar is still parsed with non-default decimalChar
  expect_identical(resource$num_undefined, expected_value) # 3000000,30
  # Field without groupChar is not parsed with non-default groupChar
  expect_identical(resource$num_undefined_group, NA_real_) # 3.000.000,30
})

test_that("read_resource() handles LF and CRLF line terminator characters", {
  # There are 3 line terminator characters:
  # LF    \n    Unix/Mac OS X
  # CR    \r    Max OS before X
  # CRLF  \r\n  Windows
  # According to spec, only LF and CRLF are allowed by default, otherwise the
  # dialect$lineTerminator should be used (with default CRLF)
  # specs.frictionlessdata.io/tabular-data-resource/#csv-file-requirements
  #
  # Line terminator characters can be checked in terminal with:
  # $ file deployments_crlf.csv
  # deployments_crlf.csv: UTF-8 Unicode text, with CRLF line terminators
  #
  # read_delim() however only handles 2 line terminator characters (LF and CRLF)
  # without explicitly indicating them, so dialect$lineTerminator is ignored
  p <- example_package()
  resource <- read_resource(p, "deployments") # This file has LF

  p_crlf <- p
  p_crlf$directory <- "."
  p_crlf$resources[[1]]$path <-
    test_path("data/deployments_crlf.csv") # File with CRLF
  expect_identical(read_resource(p_crlf, "deployments"), resource)
})

test_that("read_resource() can read compressed files", {
  skip_if_offline()
  p <- example_package()
  resource <- read_resource(p, "deployments")

  # File created in terminal with:
  # zip deployments.csv.zip deployments.csv
  p_local_zip <- p
  p_local_zip$directory <- "."
  p_local_zip$resources[[1]]$path <- test_path("data/deployments.csv.zip")
  p_remote_zip <- p
  p_remote_zip$resources[[1]]$path <- file.path(
    "https://raw.githubusercontent.com/frictionlessdata/frictionless-r",
    "main/tests/testthat/data/deployments.csv.zip"
  )

  # File created in terminal with:
  # gzip deployments.csv
  p_local_gz <- p
  p_local_gz$directory <- "."
  p_local_gz$resources[[1]]$path <- test_path("data/deployments.csv.gz")
  p_remote_gz <- p
  p_remote_gz$resources[[1]]$path <- file.path(
    "https://raw.githubusercontent.com/frictionlessdata/frictionless-r",
    "main/tests/testthat/data/deployments.csv.gz"
  )

  expect_identical(read_resource(p_local_zip, "deployments"), resource)
  # Remote zip not supported, see
  # https://github.com/tidyverse/readr/issues/1042#issuecomment-545103047
  expect_error(
    read_resource(p_remote_zip, "deployments"),
    regexp = paste(
      "Reading from remote `zip` compressed files is not supported,",
      "  download the files locally first.",
      sep = "\n"
    ),
    fixed = TRUE
  )
  expect_identical(read_resource(p_local_gz, "deployments"), resource)
  expect_identical(read_resource(p_remote_gz, "deployments"), resource)
})

test_that("read_resource() handles strings", {
  p <- read_package(test_path("data/types.json"))
  resource <- read_resource(p, "string")
  expect_type(resource$str, "character")

  # Use factor when enum is present
  enum <- p$resources[[1]]$schema$fields[[2]]$constraints$enum
  expect_s3_class(resource$str_factor, "factor")
  expect_identical(levels(resource$str_factor), enum)
})

test_that("read_resource() handles numbers", {
  p <- read_package(test_path("data/types.json"))
  resource <- read_resource(p, "number")

  # Leading/trailing zeros are optional, + is assumed
  expect_type(resource$num, "double")
  expect_true(all(resource$num == 3))
  expect_type(resource$num_neg, "double")
  expect_true(all(resource$num_neg == -3))

  # Use factor when enum is present
  enum <- p$resources[[2]]$schema$fields[[3]]$constraints$enum
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
  p <- read_package(test_path("data/types.json"))
  resource <- read_resource(p, "integer")

  # Leading/trailing zeros are optional, + is assumed
  expect_type(resource$int, "double")
  expect_true(all(resource$int == 3))
  expect_type(resource$int_neg, "double")
  expect_true(all(resource$int_neg == -3))

  # Use factor when enum is present
  enum <- p$resources[[3]]$schema$fields[[3]]$constraints$enum
  expect_s3_class(resource$int_factor, "factor")
  expect_identical(levels(resource$int_factor), as.character(enum))

  # bareNumber = false allows whitespace and non-numeric characters
  expect_type(resource$int_ws, "double")
  expect_true(all(resource$int_ws == 3))
  expect_type(resource$int_notbare, "double")
  expect_true(all(resource$int_notbare == 3))
})

test_that("read_resource() handles booleans", {
  p <- read_package(test_path("data/types.json"))
  resource <- read_resource(p, "boolean")

  # Default trueValues/falseValues are cast to logical
  expect_type(resource$bool_true, "logical")
  expect_type(resource$bool_false, "logical")
  expect_true(all(resource$bool_true == TRUE))
  expect_true(all(resource$bool_false == FALSE))
})

test_that("read_resource() handles dates", {
  expected_value <- as.Date("2013-11-23")
  p <- read_package(test_path("data/types.json"))
  resource <- read_resource(p, "date")
  # This test covers:
  # - year: %Y %y
  # - month: %m (including 1 digit) %b %B
  # - day: %d (including 1 digit) %a not %A, see
  #   https://github.com/tidyverse/readr/issues/1230
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
  p <- read_package(test_path("data/types.json"))
  resource <- read_resource(p, "time")
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
  p <- read_package(test_path("data/types.json"))
  resource <- read_resource(p, "datetime")

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
  p <- read_package(test_path("data/types.json"))
  resource <- read_resource(p, "other")

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
