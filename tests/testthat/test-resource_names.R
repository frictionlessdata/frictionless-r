test_that("resource_names() returns a character vector of resource names", {
  p <- example_package()
  expect_identical(resource_names(p), c("deployments", "observations", "media"))

  # 1 resource
  p <- remove_resource(p, "media")
  p <- remove_resource(p, "observations")
  expect_identical(resource_names(p), c("deployments"))

  # 0 resources
  p <- remove_resource(p, "deployments")
  expect_identical(resource_names(p), character(0))
})

test_that("resource_names() returns error if resources have no name", {
  p <- example_package()
  p$resources[[2]]$name <- NULL
  expect_error(
    resource_names(p),
    class = "frictionless_error_resources_without_name"
  )

  # For more tests see test-check_package.R
})
