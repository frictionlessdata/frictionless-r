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
