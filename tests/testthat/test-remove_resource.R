test_that("remove_resource() returns a valid Data Package", {
  p <- example_package
  expect_true(check_package(remove_resource(p, "deployments")))
})

test_that("remove_resource() returns error on incorrect Data Package", {
  expect_error(
    remove_resource(list(), "deployments"),
    paste(
      "`package` must be a list describing a Data Package,",
      "created with `read_package()` or `create_package()`."
    ),
    fixed = TRUE
  )
})

test_that("remove_resource() returns error when resource not found", {
  p <- example_package
  expect_error(
    remove_resource(p, "no_such_resource"),
    paste(
      "Can't find resource `no_such_resource` in `deployments`,",
      "`observations`, `media`."
    ),
    fixed = TRUE
  )
})

test_that("remove_resource() removes resource, resource_name", {
  p <- example_package

  # Remove "deployments", keep "observations" and "media
  p_removed <- remove_resource(p, "deployments")

  # Resource removed
  expect_length(p_removed$resources, 2) # Remains a list, now of length 2
  expect_identical(p_removed$resources[[1]][["name"]], "observations")

  # Resource name removed
  expect_identical(p_removed$resource_names, c("observations", "media"))
})
