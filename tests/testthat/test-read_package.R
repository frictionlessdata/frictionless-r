test_that("read_package() reads path/url and returns a list with $resource_names, $directory", {
  # Load example package (locally and remotely) and a valid minimal one
  local_path <- system.file("extdata", "datapackage.json", package = "datapackage")
  remote_url <- "https://raw.githubusercontent.com/inbo/datapackage/main/inst/extdata/datapackage.json"
  minimal_path <- "minimal_valid.json"
  local <- read_package(local_path)
  remote <- read_package(remote_url)
  minimal <- read_package(minimal_path)

  expect_type(local, "list")
  expect_type(remote, "list")
  expect_type(minimal, "list")
  resource_names <- c("deployments", "observations")
  expect_equal(local$resource_names, resource_names)
  expect_equal(remote$resource_names, resource_names)
  expect_equal(minimal$resource_names, resource_names)
  expect_equal(local$directory, gsub("/datapackage.json", "", local_path))
  expect_equal(remote$directory, gsub("/datapackage.json", "", remote_url))
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
