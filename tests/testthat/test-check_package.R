test_that("check_package() returns TRUE on valid Data Package", {
  testthat::skip_if_offline()
  expect_true(check_package(example_package))
})

test_that("check_package() returns error on incorrect Data Package", {
  # Valid package
  p <- list(
    resources = list(),
    directory = "."
  )
  expect_true(check_package(p))

  error_message <- paste(
    "`package` must be a list describing a Data Package,",
    "created with `read_package()` or `create_package()`."
  )
  # Must be a list
  expect_error(check_package("not_a_list"), error_message, fixed = TRUE)

  # Must have resources as list
  p_invalid <- p
  p_invalid$resources <- NULL
  expect_error(check_package(p_invalid), error_message, fixed = TRUE)
  p_invalid$resources <- vector(mode = "character")
  expect_error(check_package(p_invalid), error_message, fixed = TRUE)

  # Must have directory as character
  p_invalid <- p
  p_invalid$directory <- NULL
  expect_error(check_package(p_invalid), error_message, fixed = TRUE)
  p_invalid$directory <- logical()
  expect_error(check_package(p_invalid), error_message, fixed = TRUE)
})

test_that("check_package() returns error if resources have no name", {
  testthat::skip_if_offline()
  p <- example_package
  p$resources[[2]]$name <- NULL
  expect_error(
    check_package(p),
    "All resources in `package` must have property `name`",
    fixed = TRUE
  )
})
