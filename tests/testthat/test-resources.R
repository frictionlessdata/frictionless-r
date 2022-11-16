test_that("resources() returns a character vector of resource names", {
  testthat::skip_if_offline()
  p <- example_package
  expect_identical(resources(p), c("deployments", "observations", "media"))

  # 1 resource
  p <- remove_resource(p, "media")
  p <- remove_resource(p, "observations")
  expect_identical(resources(p), c("deployments"))

  # 0 resources
  p <- remove_resource(p, "deployments")
  expect_identical(resources(p), character(0))
})

test_that("resources() returns error if resources have no name", {
  p <- example_package
  p$resources[[2]]$name <- NULL
  expect_error(
    resources(p),
    "All resources in `package` must have property `name`",
    fixed = TRUE
  )

  # For more tests see test-check_package.R
})
