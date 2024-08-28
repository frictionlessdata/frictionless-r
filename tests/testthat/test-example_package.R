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

test_that("example_package() uses the correct version", {
  # version 1.0
  p_v1 <- example_package()
  p_local_v1 <- read_package(
    system.file("extdata", "v1", "datapackage.json", package = "frictionless")
  )
  expect_identical(p_v1, p_local_v1)

  # version 2.0
  p_v2 <- example_package(version = "2.0")
  p_local_v2 <- read_package(
    system.file("extdata", "v2", "datapackage.json", package = "frictionless")
  )
  expect_identical(p_v2, p_local_v2)
})
