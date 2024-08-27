test_that("example_package() returns error on invalid version", {
  expect_error(
    example_package("2"),
    class = "frictionless_error_example_package_version_invalid"
  )
  expect_error(
    example_package(2),
    class = "frictionless_error_example_package_version_invalid"
  )
  expect_error(
    example_package(2.0),
    class = "frictionless_error_example_package_version_invalid"
  )
})
