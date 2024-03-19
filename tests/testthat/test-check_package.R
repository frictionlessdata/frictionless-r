test_that("check_package() returns TRUE on valid Data Package", {
  expect_true(check_package(example_package))
})

test_that("check_package() returns error on invalid Data Package", {
  # Valid package
  p <- list(
    resources = list(),
    directory = "."
  )
  expect_true(check_package(p))

  # Check error message
  expect_error(
    check_package("not_a_list"),
    regexp = paste("`package` must be a list describing a Data Package",
                   "created with `read_package()` or `create_package()`."),
    fixed = TRUE
  )

  # Must be a list
  expect_error(
    check_package("not_a_list"),
    class = "frictionless_error_package_invalid"
  )

  # Must have resources as list
  p_invalid <- p
  p_invalid$resources <- NULL
  expect_error(
    check_package(p_invalid),
    class = "frictionless_error_package_invalid"
  )
  p_invalid$resources <- vector(mode = "character")
  expect_error(
    check_package(p_invalid),
    class = "frictionless_error_package_invalid"
  )

  # Must have directory as character
  p_invalid <- p
  p_invalid$directory <- NULL
  expect_error(
    check_package(p_invalid),
    class = "frictionless_error_package_invalid"
  )
  p_invalid$directory <- logical()
  expect_error(
    check_package(p_invalid),
    class = "frictionless_error_package_invalid"
  )
})

test_that("check_package() returns error if resources have no name", {
  p <- example_package
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
})
