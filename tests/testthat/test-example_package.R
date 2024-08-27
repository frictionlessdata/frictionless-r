test_that("example_package() returns error on invalid version", {
  expect_error(
    example_package("1"),
    class = "frictionless_error_unsupported_version"
  )
  expect_error(
    example_package("1.0.0"),
    class = "frictionless_error_unsupported_version"
  )
  expect_error(
    example_package(1.0),
    class = "frictionless_error_unsupported_version"
  )
})
