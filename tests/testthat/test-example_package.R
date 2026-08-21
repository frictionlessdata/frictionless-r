test_that("example_package() allows version selection (default 1.0)", {
  p_v1 <- read_package(
    system.file("extdata", "v1", "datapackage.json", package = "frictionless")
  )
  p_v2 <- read_package(
    system.file("extdata", "v2", "datapackage.json", package = "frictionless")
  )

  expect_identical(example_package("1.0"), p_v1)
  expect_identical(example_package("2.0"), p_v2)
  expect_identical(example_package("not_a_version"), p_v1)
  expect_identical(example_package(version = NULL), p_v1)
})
