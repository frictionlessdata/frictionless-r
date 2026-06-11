test_that("get_schema() shows deprecation warning", {
  p <- example_package()
  lifecycle::expect_deprecated(get_schema(p, "deployments"))
})

test_that("get_schema() forwards to schema()", {
  withr::local_options(lifecycle_verbosity = "quiet")
  p <- example_package()
  expect_identical(
    get_schema(p, "deployments"),
    schema(p, "deployments")
  )
})

test_that("resources() shows deprecation warning", {
  p <- example_package()
  lifecycle::expect_deprecated(resources(p))
})

test_that("resources() forwards to resource_names()", {
  withr::local_options(lifecycle_verbosity = "quiet")
  p <- example_package()
  expect_identical(
    resources(p),
    resource_names(p)
  )
})
