test_that("create_package() initiates a valid data package", {
  expect_true(check_package(create_package()))
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

test_that("create_package() sets resources if not provided", {
  new <- create_package()
  expect_identical(new$resources, list())

  existing <- create_package(list(resources = "not_default"))
  expect_identical(existing$resources, "not_default")
})

test_that("create_package() sets directory if not provided", {
  new <- create_package()
  expect_identical(new$directory, ".")

  existing <- create_package(list(directory = "not_default"))
  expect_identical(existing$directory, "not_default")
})

test_that("create_package() adds class 'datapackage'", {
  new <- create_package()
  expect_s3_class(new, "datapackage")

  existing <- create_package(list())
  expect_s3_class(new, "datapackage")
})
