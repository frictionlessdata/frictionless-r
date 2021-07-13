test_that("read_resource() returns error on incorrect package", {
  expect_error(
    read_resource("not_a_list", "dep"), "`package` must be a list object"
  )
  expect_error(
    read_resource(list(), "dep"), "`package` must have property `resource_names`"
  )
})

test_that("read_resource() returns error on incorrect resource", {
  pkg <- suppressMessages(read_package(
    system.file("extdata", "datapackage.json", package = "datapackage"))
  )
  expect_error(read_resource(pkg, "no_such_resource"), "Can't find resource")

  # Create invalid package and add properties one by one to pass errors
  pkg_invalid <- list(resource_names = c("deployments"),
                  resources = list(list(name = "deployments")))
  expect_error(
    read_resource(pkg_invalid, "deployments"),
    "must have property `profile` with value `tabular-data-resource`"
  )

  pkg_invalid$resources[[1]]$profile <- "tabular-data-resource"
  expect_error(
    read_resource(pkg_invalid, "deployments"), "must have property `path`"
  )

  pkg_invalid$resources[[1]]$path <- "http://example.com/no_file.csv"
  expect_error(
    read_resource(pkg_invalid, "deployments"), "Can't find file at `http:"
  )

  pkg_invalid$resources[[1]]$path <- "no_file.csv"
  expect_error(
    read_resource(pkg_invalid, "deployments"), "Can't find file at `/no_file.csv"
  )

  pkg_invalid$resources[[1]]$path <- c("deployments.csv", "no_file.csv")
  expect_error(read_resource(pkg_invalid, "deployments"), "Can't find file at")

  pkg_invalid$resources[[1]]$path <- "/inst/extdata/deployments.csv"
  expect_error(
    read_resource(pkg_invalid, "deployments"), "is an absolute path"
  )

  pkg_invalid$resources[[1]]$path <- "../../inst/extdata/deployments.csv"
  expect_error(
    read_resource(pkg_invalid, "deployments"), "is a relative parent path"
  )

  pkg_invalid$resources[[1]]$path <- "deployments.csv"
  pkg_invalid$directory <- dirname(system.file("extdata", "datapackage.json", package = "datapackage"))
  expect_error(
    read_resource(pkg_invalid, "deployments"), "must have property `schema`"
  )

  pkg_invalid$resources[[1]]$schema$fields = list(
    list(name = "deployment_id"), # Field 1
    list(type = "number") # Field 2
  )
  expect_error(
    read_resource(pkg_invalid, "deployments"),
    "Field 2 of resource `deployments` must have the property `name`."
  )
})

test_that("read_resource() returns a tibble", {
  pkg <- suppressMessages(read_package(
    system.file("extdata", "datapackage.json", package = "datapackage"))
  )
  resource <- read_resource(pkg, "deployments")

  expect_s3_class(resource, "data.frame")
  expect_s3_class(resource, "tbl")
})

test_that("read_resource() understands CSV dialect", {
  pkg <- suppressMessages(read_package(
    system.file("extdata", "datapackage.json", package = "datapackage"))
  )
  resource <- read_resource(pkg, "deployments")

  # Create package with non-default dialect properties
  pkg_dialect <- pkg
  pkg_dialect$directory <- "." # Use "./tests/testthat" outside test
  pkg_dialect$resources[[1]]$path <- "deployments_dialect.csv"
  pkg_dialect$resources[[1]]$dialect <- list(
    delimiter = ":",
    # lineTerminator
    quoteChar = "'",         # Used to wrap dates which contain delimiter ":"
    doubleQuote = TRUE,      # Will get set to FALSE because of escapeChar
    escapeChar = "\\",       # Used to escape ":" in comments
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
  attr(resource_dialect, 'spec')$skip <- 1

  expect_identical(resource, resource_dialect)
})

test_that("read_resource() understands missing values", {
  pkg <- suppressMessages(read_package(
    system.file("extdata", "datapackage.json", package = "datapackage"))
  )
  resource <- read_resource(pkg, "deployments")

  # Create package with non-default missing values
  pkg_missing <- pkg
  pkg_missing$directory <- "." # Use "./tests/testthat" outside test
  pkg_missing$resources[[1]]$path <- "deployments_missingvalues.csv"
  pkg_missing$resources[[1]]$schema$missingValues <-
    append(pkg_missing$resources[[1]]$schema$missingValues, "ignore")
  resource_missing <- read_resource(pkg_missing, "deployments")

  expect_identical(resource, resource_missing)
})

test_that("read_resource() understands encoding", {
  pkg <- suppressMessages(read_package(
    system.file("extdata", "datapackage.json", package = "datapackage"))
  )
  resource <- read_resource(pkg, "deployments")

  # Create package with non-default missing values
  pkg_encoding <- pkg
  pkg_encoding$directory <- "." # Use "./tests/testthat" outside test
  pkg_encoding$resources[[1]]$path <- "deployments_encoding.csv"
  pkg_encoding$resources[[1]]$encoding <- "windows-1252"
  resource_encoding <- read_resource(pkg_encoding, "deployments")

  expect_identical(resource, resource_encoding)
})

test_that("read_resource() handles LF, CR and CRLF line endings", {
  # There are 3 line endings:
  # LF    \n    Unix/Mac OS X
  # CR    \r    Max OS before X
  # CRLF  \r\n  Windows
  # According to spec, only LF and CRLF are allowed by default, otherwise the
  # dialect$lineTerminator should be used (with default CRLF)
  # https://specs.frictionlessdata.io/tabular-data-resource/#csv-file-requirements
  #
  # Line endings can be checked in terminal with:
  #$ file deployments_cr.csv
  #deployments_cr.csv: UTF-8 Unicode text, with CR line terminators
  #
  # read_delim() however handles all 3 line endings with explicitly indicating,
  # so dialect$lineTerminator is ignored
  pkg <- suppressMessages(read_package(
    system.file("extdata", "datapackage.json", package = "datapackage"))
  )
  resource <- read_resource(pkg, "deployments") # This file has LF

  pkg_cr <- pkg
  pkg_cr$directory <- "." # Use "./tests/testthat" outside test
  pkg_cr$resources[[1]]$path <- "deployments_cr.csv" # This file has CR
  resource_cr <- read_resource(pkg_cr, "deployments")

  pkg_crlf <- pkg
  pkg_crlf$directory <- "." # Use "./tests/testthat" outside test
  pkg_crlf$resources[[1]]$path <- "deployments_cr.csv" # This file has CRLF
  resource_crlf <- read_resource(pkg_crlf, "deployments")

  expect_identical(resource, resource_cr)
  expect_identical(resource, resource_crlf)
})

test_that("read_resource() can read compressed files", {
  pkg <- suppressMessages(read_package(
    system.file("extdata", "datapackage.json", package = "datapackage"))
  )
  resource <- read_resource(pkg, "deployments")

  # File created in terminal with:
  # zip deployments.csv.zip deployments.csv
  pkg_local_zip <- pkg
  pkg_local_zip$directory <- "." # Use "./tests/testthat" outside test
  pkg_local_zip$resources[[1]]$path <- "deployments.csv.zip"
  pkg_remote_zip <- pkg
  pkg_remote_zip$resources[[1]]$path <-
    "https://github.com/inbo/datapackage/raw/main/tests/testthat/deployments.csv.zip"

  # File created in terminal with:
  # gzip deployments.csv
  pkg_local_gz <- pkg
  pkg_local_gz$directory <- "." # Use "./tests/testthat" outside test
  pkg_local_gz$resources[[1]]$path <- "deployments.csv.gz"
  pkg_remote_gz <- pkg
  pkg_remote_gz$resources[[1]]$path <-
    "https://github.com/inbo/datapackage/raw/main/tests/testthat/deployments.csv.gz"

  expect_identical(resource, read_resource(pkg_local_zip, "deployments"))
  # Remote zip not supported, see
  # https://github.com/tidyverse/readr/issues/1042#issuecomment-545103047
  expect_error(read_resource(pkg_remote_zip, "deployments"))
  expect_identical(resource, read_resource(pkg_local_gz, "deployments"))
  expect_identical(resource, read_resource(pkg_remote_gz, "deployments"))
})

test_that("read_resource() handles strings", {
  # See https://specs.frictionlessdata.io/table-schema/#string
  pkg <- suppressMessages(read_package("types.json"))
  resource <- read_resource(pkg, "string")

  expect_type(resource$str, "character")

  # Use factor when enum is present
  enum <- pkg$resources[[1]]$schema$fields[[2]]$constraints$enum
  expect_s3_class(resource$str_factor, "factor")
  expect_equal(levels(resource$str_factor), enum)
})

test_that("read_resource() handles numbers", {
  pkg <- suppressMessages(read_package("types.json"))
  resource <- read_resource(pkg, "number")

  # Leading/trailing zeros are optional, + is assumed
  expect_type(resource$num, "double")
  expect_true(all(resource$num == 3))
  expect_type(resource$num_neg, "double")
  expect_true(all(resource$num_neg == -3))

  # Use factor when enum is present
  enum <- pkg$resources[[2]]$schema$fields[[3]]$constraints$enum
  expect_s3_class(resource$num_factor, "factor")
  expect_equal(levels(resource$num_factor), as.character(enum))

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
  pkg <- suppressMessages(read_package("types.json"))
  resource <- read_resource(pkg, "integer")

  # Leading/trailing zeros are optional, + is assumed
  expect_type(resource$int, "double")
  expect_true(all(resource$int == 3))
  expect_type(resource$int_neg, "double")
  expect_true(all(resource$int_neg == -3))

  # Use factor when enum is present
  enum <- pkg$resources[[3]]$schema$fields[[3]]$constraints$enum
  expect_s3_class(resource$int_factor, "factor")
  expect_equal(levels(resource$int_factor), as.character(enum))

  # bareNumber = false allows whitespace and non-numeric characters
  expect_type(resource$int_ws, "double")
  expect_true(all(resource$int_ws == 3))
  expect_type(resource$int_notbare, "double")
  expect_true(all(resource$int_notbare == 3))
})

test_that("read_resource() handles booleans", {
  pkg <- suppressMessages(read_package("types.json"))
  resource <- read_resource(pkg, "boolean")

  # Default trueValues/falseValues are cast to logical
  expect_type(resource$bool_true, "logical")
  expect_type(resource$bool_false, "logical")
  expect_true(all(resource$bool_true == TRUE))
  expect_true(all(resource$bool_false == FALSE))
})

test_that("read_resource() handles other types", {
  pkg <- suppressMessages(read_package("types.json"))
  resource <- read_resource(pkg, "other")

  # Interpret year, yearmonth as factor
  expect_s3_class(resource$year, "factor")
  expect_equal(levels(resource$year), c("2001", "2002"))
  expect_s3_class(resource$yearmonth, "factor")
  expect_equal(levels(resource$yearmonth), c("2001-03", "2001-04"))

  # Interpret object, array, geopoint, geojson as character
  expect_type(resource$object, "character")
  expect_type(resource$array, "character")
  expect_type(resource$geopoint, "character")
  expect_type(resource$geojson, "character")

  # Interpret any as character
  expect_type(resource$any, "character")

  # Guess undefined or unknown types
  expect_type(resource$no_type, "logical")
  expect_type(resource$unknown_type, "logical")
})
