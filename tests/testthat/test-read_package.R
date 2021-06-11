test_that("read_package() reads path/url and returns a list with $resource_names, $directory", {
  # Load example package (locally and remotely) and a valid minimal one
  example_path <- system.file("extdata", "datapackage.json", package = "datapackage")
  example_url <- "https://raw.githubusercontent.com/inbo/datapackage/main/inst/extdata/datapackage.json"
  minimal_path <- "minimal_valid.json"
  example_local <- read_package(example_path)
  example_remote <- read_package(example_url)
  minimal <- read_package(minimal_path)

  expect_type(example_local, "list")
  expect_type(example_remote, "list")
  expect_type(minimal, "list")
  resource_names <- c("deployments", "observations")
  expect_equal(example_local$resource_names, resource_names)
  expect_equal(example_remote$resource_names, resource_names)
  expect_equal(minimal$resource_names, resource_names)
  expect_equal(example_local$directory, gsub("/datapackage.json", "", example_path))
  expect_equal(example_remote$directory, gsub("/datapackage.json", "", example_url))
  expect_equal(minimal$directory, ".")
})

test_that("read_package() returns error on missing file and properties", {
  # No file
  expect_error(read_package("nofile.json"), "Can't find file at")
  expect_error(
    read_package("http://example.com/nofile.json"),
    "Can't find file at"
  )
  # Not a json file: parsing error
  expect_error(read_package(system.file("extdata", "deployments.csv", package = "datapackage")))
  # No resource name (same would happen on no resources)
  expect_error(
    read_package("no_resource_name.json"),
    "must have property `resources` containing at least one resource with a `name`"
  )
})
