test_that("create_package() initiates a valid data package", {
  expect_true(check_package(create_package()))
})

test_that("create_package() sets profile (to 'tabular-data-package')", {
  new <- create_package()
  expect_identical(new$profile, "tabular-data-package")

  existing <- create_package(list(profile = "not_default"))
  expect_identical(existing$profile, "not_default")
})

test_that("create_package() sets resources", {
  new <- create_package()
  expect_identical(new$resources, list())

  existing <- create_package(list(resources = "not_default"))
  expect_identical(existing$resources, "not_default")
})

test_that("create_package() sets directory", {
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
