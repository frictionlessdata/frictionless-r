test_that("check_package() returns error on incorrect package", {
  # Valid package
  pkg <- list(
    resources = list(),
    resource_names = vector(mode = "character"),
    directory = "."
  )
  class(pkg) <- c("datapackage", class(pkg))
  expect_true(check_package(pkg))

  # Must be a list
  expect_error(check_package("not_a_list"))

  # Must have class datapackage
  pkg_invalid <- pkg
  class(pkg_invalid) = "list"
  expect_error(check_package(pkg_invalid))

  # Must have resources as list
  pkg_invalid <- pkg
  pkg_invalid$resources <- NULL
  expect_error(check_package(pkg_invalid))
  pkg_invalid$resources <- vector(mode = "character")
  expect_error(check_package(pkg_invalid))

  # Must have resource names as character vector
  pkg_invalid <- pkg
  pkg_invalid$resource_names <- NULL
  expect_error(check_package(pkg_invalid))
  pkg_invalid$resource_names <- list()
  expect_error(check_package(pkg_invalid))

  # Must have directory as character
  pkg_invalid <- pkg
  pkg_invalid$directory <- NULL
  expect_error(check_package(pkg_invalid))
  pkg_invalid$directory <- logical()
  expect_error(check_package(pkg_invalid))
})