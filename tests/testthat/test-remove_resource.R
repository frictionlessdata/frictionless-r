test_that("remove_resource() returns error on incorrect package", {
  expect_error(
    remove_resource("deployments", list()),
    "`package` must be a list object of class `datapackage`"
  )
})

test_that("remove_resource() returns error when resource not found", {
  pkg <- suppressMessages(read_package(
    system.file("extdata", "datapackage.json", package = "frictionless"))
  )
  expect_error(remove_resource("no_such_resource", pkg), "Can't find resource")
})

test_that("remove_resource() removes resource, resource_name and returns package", {
  pkg <- suppressMessages(read_package(
    system.file("extdata", "datapackage.json", package = "frictionless"))
  )
  # Remove "deployments", keep "observations"
  pkg_removed <- remove_resource("deployments", pkg)

  # Returns a valid package
  expect_true(check_package(pkg_removed))

  # Resource removed
  expect_length(pkg_removed$resources, 1) # Remains a list, now of length 1
  expect_equal(pkg_removed$resources[[1]][["name"]], "observations")

  # Resource name removed
  expect_length(pkg_removed$resource_names, 1)
  expect_equal(pkg_removed$resource_names, "observations")
})
