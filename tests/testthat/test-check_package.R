test_that("check_package() returns TRUE on valid Data Package", {
  expect_true(check_package(example_package))
})

test_that("check_package() returns error on incorrect Data Package", {
  # Valid package
  p <- list(
    resources = list(),
    resource_names = vector(mode = "character"),
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

  # Must have resource names as character vector
  p_invalid <- p
  p_invalid$resource_names <- NULL
  expect_error(check_package(p_invalid), error_message, fixed = TRUE)
  p_invalid$resource_names <- list()
  expect_error(check_package(p_invalid), error_message, fixed = TRUE)

  # Must have directory as character
  p_invalid <- p
  p_invalid$directory <- NULL
  expect_error(check_package(p_invalid), error_message, fixed = TRUE)
  p_invalid$directory <- logical()
  expect_error(check_package(p_invalid), error_message, fixed = TRUE)
})

test_that("check_package() returns error if resources have no name", {
  p <- example_package
  p$resources[[2]]$name <- NULL
  expect_error(
    check_package(p),
    "All resources in `package` must have property `name`",
    fixed = TRUE
  )
})

test_that("check_package() returns error if resource_names are out of sync", {
  p <- example_package
  p$resource_names <- c("no_such_resource", "observations", "media")
  expect_error(
    check_package(p),
    paste(
      "Can't find resource(s) with name(s) `no_such_resource`.",
      "ℹ Is `package$resource_names` out of sync with `name` of resources?",
      sep = "\n"
    ),
    fixed = TRUE
  )
  p$resource_names <- c("no_such_resource", "neither", "media")
  expect_error(
    check_package(p),
    paste(
      "Can't find resource(s) with name(s) `no_such_resource`, `neither`.",
      "ℹ Is `package$resource_names` out of sync with `name` of resources?",
      sep = "\n"
    ),
    fixed = TRUE
  )
})
