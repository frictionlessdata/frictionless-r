test_that("create_package() creates a valid data package or returns error", {
  new <- create_package()
  expect_no_error(check_package(new))

  existing <- create_package(
    list(resources = list(), directory = "not_default")
  )
  expect_no_error(check_package(existing))

  expect_error(
    create_package(list(resources = "not_a_list")),
    class = "frictionless_error_package_invalid"
  )
})

test_that("create_package() returns error on invalid descriptor", {
  expect_error(
    create_package("not_a_list"),
    class = "frictionless_error_descriptor_invalid"
  )
  expect_error(
    create_package("not_a_list"),
    regexp = "`descriptor` must be a list if provided."
  )
})

test_that("create_package() sets resources or leaves as is", {
  new <- create_package()
  expect_identical(new$resources, list())

  custom_resources <- list(list("name" = "custom_name"))
  existing <- create_package(list(resources = custom_resources))
  expect_identical(existing$resources, custom_resources)
})

test_that("create_package() sets directory or leaves as is", {
  new <- create_package()
  expect_identical(attr(new, "directory"), ".")

  list_with_directory <- list()
  attr(list_with_directory, "directory") <- "not_default"
  existing <- create_package(list_with_directory)
  expect_identical(attr(existing, "directory"), "not_default")
})

test_that("create_package() adds class 'datapackage'", {
  new <- create_package()
  expect_s3_class(new, "datapackage")

  existing <- create_package(list())
  expect_s3_class(new, "datapackage")
})
