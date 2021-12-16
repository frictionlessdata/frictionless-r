test_that("remove_resource() returns a valid Data Package", {
  pkg <- example_package
  expect_true(check_package(remove_resource(pkg, "deployments")))
})

test_that("remove_resource() returns error on incorrect Data Package", {
  expect_error(
    remove_resource(list(), "deployments"),
    paste(
      "`package` must be a list object of class `datapackage` created with",
      "`read_package()` or `create_package()`."
    ),
    fixed = TRUE
  )
})

test_that("remove_resource() returns error when resource not found", {
  pkg <- example_package
  expect_error(
    remove_resource(pkg, "no_such_resource"),
    paste(
      "Can't find resource `no_such_resource` in `deployments`,",
      "`observations`, `media`."
    ),
    fixed = TRUE
  )
})

test_that("remove_resource() removes resource, resource_name", {
  pkg <- example_package

  # Remove "deployments", keep "observations" and "media
  pkg_removed <- remove_resource(pkg, "deployments")

  # Resource removed
  expect_length(pkg_removed$resources, 2) # Remains a list, now of length 2
  expect_equal(pkg_removed$resources[[1]][["name"]], "observations")

  # Resource name removed
  expect_equal(pkg_removed$resource_names, c("observations", "media"))
})
