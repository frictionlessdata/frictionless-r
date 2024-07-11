test_that("check_package() returns package invisibly on valid Data Package", {
  p <- example_package()
  expect_identical(check_package(p), p)
  expect_invisible(check_package(p))
})

test_that("check_package() returns error on invalid Data Package", {
  expect_error(
    check_package("not_valid"),
    class = "frictionless_error_package_invalid"
  )
  expect_error(
    check_package("not_valid"),
    regexp = "`package` must be a Data Package object.",
    fixed = TRUE
  )
  expect_error(
    check_package("not_valid"),
    regexp = paste(
      "Create a valid Data Package object with `read_package()` or",
      "`create_package()`."
    ),
    fixed = TRUE
  )
})

test_that("check_package() returns error if package is not a list", {
  expect_error(
    check_package("not_a_list"),
    class = "frictionless_error_package_invalid"
  )
  expect_error(
    check_package("not_a_list"),
    regexp = "`package` is not a list.",
    fixed = TRUE
  )
})

test_that("check_package() returns error on missing or incorrect resources", {
  expect_error(
    check_package(list()),
    class = "frictionless_error_package_invalid"
  )
  expect_error(
    check_package(list(resources = "not_a_list")),
    regexp = "`package` is missing a resources property or it is not a list.",
    fixed = TRUE
  )
  expect_error(
    check_package(list(resources = "not_a_list")),
    class = "frictionless_error_package_invalid"
  )
})

test_that("check_package() returns error on missing or incorrect directory", {
  expect_error(
    check_package(list(resources = list())),
    class = "frictionless_error_package_invalid"
  )
  expect_error(
    check_package(list(resources = list())),
    regexp = paste(
      "`package` is missing a directory property or it is not a character."
    ),
    fixed = TRUE
  )
  expect_error(
    check_package(list(resources = list(), directory = 5)),
    class = "frictionless_error_package_invalid"
  )
})

test_that("check_package() returns error if resources have no name", {
  p <- example_package()
  p$resources[[2]]$name <- NULL
  expect_error(
    check_package(p),
    class = "frictionless_error_resources_without_name"
  )
  expect_error(
    check_package(p),
    regexp = "All resources in `package` must have a name property.",
    fixed = TRUE
  )

  # Expect no error on empty resources
  p$resources <- list()
  expect_no_error(check_package(p))
})

test_that("check_package() returns warning on missing 'datapackage' class", {
  expect_warning(
    check_package(list(resources = list(), directory = ".")),
    class = "frictionless_warning_package_without_class"
  )
  expect_warning(
    check_package(list(resources = list(), directory = ".")),
    regexp = "`package` is missing a \"datapackage\" class",
    fixed = TRUE
  )
})
