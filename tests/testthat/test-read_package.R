test_that("read_package() returns a valid Data Package, whether reading path or url", {
  # Load example package (locally and remotely) and a valid minimal one
  pkg_path <- system.file("extdata", "datapackage.json", package = "frictionless")
  pkg_url <- "https://raw.githubusercontent.com/frictionlessdata/frictionless-r/main/inst/extdata/datapackage.json"
  minimal_path <- "data/valid_minimal.json"
  pkg_local <- suppressMessages(read_package(pkg_path))
  pkg_remote <- suppressMessages(read_package(pkg_url))
  pkg_minimal <- suppressMessages(read_package(minimal_path))

  # Returns a list of class "datapackage", with at minimal resources,
  # resource_names and directory
  expect_true(check_package(pkg_local))
  expect_true(check_package(pkg_remote))
  expect_true(check_package(pkg_minimal))

  # Package has correct "resource_names"
  resource_names <- c("deployments", "observations", "media")
  expect_equal(pkg_local$resource_names, resource_names)
  expect_equal(pkg_remote$resource_names, resource_names)
  expect_equal(pkg_minimal$resource_names, resource_names)

  # Package has correct "directory", containing root dir of datapackage.json
  expect_equal(pkg_local$directory, gsub("/datapackage.json", "", pkg_path))
  expect_equal(pkg_remote$directory, gsub("/datapackage.json", "", pkg_url))
  expect_equal(pkg_minimal$directory, "data")
})

test_that("read_package() informs about usage norms", {
  # Load example package and a minimal valid one a URL in "id"
  pkg_path <- system.file("extdata", "datapackage.json", package = "frictionless")
  minimal_extra_path <- "data/valid_minimal_extra.json"

  expect_message(read_package(pkg_path), "make sure you have the right to")
  expect_message(
    read_package(minimal_extra_path),
    "For more information, see https://example.com"
  )
})

test_that("read_package() returns error on missing file and properties", {
  # No file
  expect_error(read_package("nofile.json"), "Can't find file at")
  expect_error(
    read_package("http://example.com/nofile.json"),
    "Can't find file at"
  )

  # Not a json file: parsing error
  expect_error(read_package(system.file("extdata", "deployments.csv", package = "frictionless")))

  # No resources
  expected_error_msg <- "must have property `resources` containing at least one resource. All resources must have a `name`."
  expect_error(read_package("data/resources_missing.json"), expected_error_msg)

  # Empty resources
  expect_error(read_package("data/resources_empty.json"), expected_error_msg)

  # No resource name
  expect_error(read_package("data/resources_no_name.json"), expected_error_msg)
})
