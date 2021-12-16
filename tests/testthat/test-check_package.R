test_that("check_package() returns TRUE on valid Data Package", {
  expect_true(check_package(example_package))
})

test_that("check_package() returns error on incorrect Data Package", {
  # Valid package
  pkg <- list(
    resources = list(),
    resource_names = vector(mode = "character"),
    directory = "."
  )
  class(pkg) <- c("datapackage", class(pkg))
  expect_true(check_package(pkg), "`package` must be")

  # Must be a list
  expect_error(check_package("not_a_list"), "`package` must be")

  # Must have class datapackage
  pkg_invalid <- pkg
  class(pkg_invalid) <- "list"
  expect_error(check_package(pkg_invalid), "`package` must be")

  # Must have resources as list
  pkg_invalid <- pkg
  pkg_invalid$resources <- NULL
  expect_error(check_package(pkg_invalid), "`package` must be")
  pkg_invalid$resources <- vector(mode = "character")
  expect_error(check_package(pkg_invalid), "`package` must be")

  # Must have resource names as character vector
  pkg_invalid <- pkg
  pkg_invalid$resource_names <- NULL
  expect_error(check_package(pkg_invalid), "`package` must be")
  pkg_invalid$resource_names <- list()
  expect_error(check_package(pkg_invalid), "`package` must be")

  # Must have directory as character
  pkg_invalid <- pkg
  pkg_invalid$directory <- NULL
  expect_error(check_package(pkg_invalid), "`package` must be")
  pkg_invalid$directory <- logical()
  expect_error(check_package(pkg_invalid), "`package` must be")
})

test_that("check_package() returns error if resources have no name", {
  pkg <- example_package
  pkg$resources[[2]]$name <- NULL
  expect_error(
    check_package(pkg),
    "All resources in `package` must have property `name`"
  )
})

test_that("check_package() returns error if resource_names are out of sync", {
  pkg <- example_package
  pkg$resource_names <- c("no_such_resource", "observations", "media")
  expect_error(
    check_package(pkg),
    "Can't find resource\\(s\\) with name\\(s\\) `no_such_resource`."
  )
  pkg$resource_names <- c("no_such_resource", "no_such_either", "media")
  expect_error(
    check_package(pkg),
    "Can't find resource\\(s\\) with name\\(s\\) `no_such_resource`, `no_such_either`."
  )
})
