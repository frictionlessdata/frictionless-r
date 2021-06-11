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

  # Create a invalid data package descriptor and add properties one by one to
  # pass errors
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
  # Test for multiple paths
  invalid$resources[[1]]$path <- c("deployments.csv", "no_file.csv")
  expect_error(read_resource(invalid, "deployments"), "Can't find file at")
})

test_that("read_resource() understands CSV dialect properties", {
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
