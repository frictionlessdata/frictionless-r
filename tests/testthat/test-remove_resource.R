test_that("remove_resource() returns a valid Data Package", {
  skip_if_offline()
  p <- example_package
  expect_no_error(check_package(remove_resource(p, "deployments")))
})

test_that("remove_resource() returns error on invalid Data Package", {
  expect_error(
    remove_resource(list(), "deployments"),
    class = "frictionless_error_package_invalid"
  )
})

test_that("remove_resource() returns error when resource not found", {
  skip_if_offline()
  p <- example_package
  expect_error(
    remove_resource(p, "no_such_resource"),
    class = "frictionless_error_resource_not_found"
  )
  expect_error(
    remove_resource(p, "no_such_resource"),
    regexp = "Can't find resource \"no_such_resource\" in `package`",
    fixed = TRUE
  )
  expect_error(
    remove_resource(p, "no_such_resource"),
    regexp = "Available resources: \"deployments\", \"observations\", and \"media\".",
    fixed = TRUE
  )
})

test_that("remove_resource() removes resource", {
  skip_if_offline()
  p <- example_package

  # Remove "deployments", keep "observations" and "media
  p_removed <- remove_resource(p, "deployments")

  # Resource removed
  expect_length(p_removed$resources, 2) # Remains a list, now of length 2
  expect_identical(p_removed$resources[[1]][["name"]], "observations")
})
