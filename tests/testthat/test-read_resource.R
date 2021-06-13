test_that("read_resource() returns error on incorrect package", {
  expect_error(
    read_resource("not_a_list", "obs"), "`package` must be a list object"
  )
  expect_error(
    read_resource(list(), "obs"), "`package` must have property `resource_names`"
  )
})

test_that("read_resource() returns error on incorrect resource", {
  example_local <- read_package(system.file("extdata", "datapackage.json", package = "datapackage"))
  expect_error(read_resource(example_local, "no_such_resource"), "Can't find resource")

  # Create invalid package and add properties one by one to pass errors
  invalid <- list(resource_names = c("deployments"),
                  resources = list(list(name = "deployments")))
  expect_error(
    read_resource(invalid, "deployments"),
    "must have property `profile` with value `tabular-data-resource`"
  )

  invalid$resources[[1]]$profile <- "tabular-data-resource"
  expect_error(
    read_resource(invalid, "deployments"), "must have property `path`"
  )

  invalid$resources[[1]]$path <- "http://example.com/no_file.csv"
  expect_error(
    read_resource(invalid, "deployments"), "Can't find file at `http:"
  )

  invalid$resources[[1]]$path <- "no_file.csv"
  expect_error(
    read_resource(invalid, "deployments"), "Can't find file at `/no_file.csv"
  )

  invalid$resources[[1]]$path <- c("deployments.csv", "no_file.csv")
  expect_error(read_resource(invalid, "deployments"), "Can't find file at")

  invalid$resources[[1]]$path <- "/inst/extdata/deployments.csv"
  expect_error(
    read_resource(invalid, "deployments"), "is an absolute path"
  )

  invalid$resources[[1]]$path <- "../../inst/extdata/deployments.csv"
  expect_error(
    read_resource(invalid, "deployments"), "is a relative parent path"
  )

  invalid$resources[[1]]$path <- "deployments.csv"
  invalid$directory <- dirname(system.file("extdata", "datapackage.json", package = "datapackage"))
  expect_error(
    read_resource(invalid, "deployments"), "must have property `schema`"
  )

  invalid$resources[[1]]$schema$fields = list(
    list(name = "deployment_id"), # Field 1
    list(type = "number") # Field 2
  )
  expect_error(
    read_resource(invalid, "deployments"),
    "Field 2 of resource `deployments` must have the property `name`."
  )
})

test_that("read_resource() returns a tibble", {
  example <- read_package(system.file("extdata", "datapackage.json", package = "datapackage"))
  example_df <- read_resource(example, "deployments")

  expect_s3_class(example_df, "data.frame")
  expect_s3_class(example_df, "tbl")
})

test_that("read_resource() understands CSV dialect", {
  example <- read_package(system.file("extdata", "datapackage.json", package = "datapackage"))
  example_df <- read_resource(example, "deployments")

  # Create package with non-default dialect properties
  example_dialect <- example
  example_dialect$directory <- "." # Use "./tests/testthat" outside test
  example_dialect$resources[[1]]$path <- "deployments_dialect.csv"
  example_dialect$resources[[1]]$dialect <- list(
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
  example_dialect_df <- read_resource(example_dialect, "deployments")
  # One attribute of this df will be different: skip = 0 (since no header)
  # The default read_resource() sets this to: skip = 1
  # Since that is not a difference we want to test, we overwrite it
  attr(example_dialect_df, 'spec')$skip <- 1

  expect_identical(example_df, example_dialect_df)
})

test_that("read_resource() understands missing values", {
  example <- read_package(system.file("extdata", "datapackage.json", package = "datapackage"))
  example_df <- read_resource(example, "deployments")

  # Create package with non-default missing values
  example_missing <- example
  example_missing$directory <- "." # Use "./tests/testthat" outside test
  example_missing$resources[[1]]$path <- "deployments_missingvalues.csv"
  example_missing$resources[[1]]$schema$missingValues <-
    append(example_missing$resources[[1]]$schema$missingValues, "ignore")
  example_missing_df <- read_resource(example_missing, "deployments")

  expect_identical(example_df, example_missing_df)
})

test_that("read_resource() understands encoding", {
  example <- read_package(system.file("extdata", "datapackage.json", package = "datapackage"))
  example_df <- read_resource(example, "deployments")

  # Create package with non-default missing values
  example_encoding <- example
  example_encoding$directory <- "." # Use "./tests/testthat" outside test
  example_encoding$resources[[1]]$path <- "deployments_encoding.csv"
  example_encoding$resources[[1]]$encoding <- "windows-1252"
  example_encoding_df <- read_resource(example_encoding, "deployments")

  expect_identical(example_df, example_encoding_df)
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
  example <- read_package(system.file("extdata", "datapackage.json", package = "datapackage"))
  example_df <- read_resource(example, "deployments") # This file has LF

  example_cr <- example
  example_cr$directory <- "." # Use "./tests/testthat" outside test
  example_cr$resources[[1]]$path <- "deployments_cr.csv" # This file has CR
  example_cr_df <- read_resource(example_cr, "deployments")

  example_crlf <- example
  example_crlf$directory <- "." # Use "./tests/testthat" outside test
  example_crlf$resources[[1]]$path <- "deployments_cr.csv" # This file has CRLF
  example_crlf_df <- read_resource(example_crlf, "deployments")

  expect_identical(example_df, example_cr_df)
  expect_identical(example_df, example_crlf_df)
})

test_that("read_resource() can read compressed files", {
  example <- read_package(system.file("extdata", "datapackage.json", package = "datapackage"))
  example_df <- read_resource(example, "deployments")

  # File created in terminal with:
  # zip deployments.csv.zip deployments.csv
  example_local_zip <- example
  example_local_zip$directory <- "." # Use "./tests/testthat" outside test
  example_local_zip$resources[[1]]$path <- "deployments.csv.zip"
  example_remote_zip <- example
  example_remote_zip$resources[[1]]$path <-
    "https://github.com/inbo/datapackage/raw/main/tests/testthat/deployments.csv.zip"

  # File created in terminal with:
  # gzip deployments.csv
  example_local_gz <- example
  example_local_gz$directory <- "." # Use "./tests/testthat" outside test
  example_local_gz$resources[[1]]$path <- "deployments.csv.gz"
  example_remote_gz <- example
  example_remote_gz$resources[[1]]$path <-
    "https://github.com/inbo/datapackage/raw/main/tests/testthat/deployments.csv.gz"

  expect_identical(example_df, read_resource(example_local_zip, "deployments"))
  # Remote zip not supported, see
  # https://github.com/tidyverse/readr/issues/1042#issuecomment-545103047
  expect_error(read_resource(example_remote_zip, "deployments"))
  expect_identical(example_df, read_resource(example_local_gz, "deployments"))
  expect_identical(example_df, read_resource(example_remote_gz, "deployments"))
})
