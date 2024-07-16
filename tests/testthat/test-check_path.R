test_that("check_path() returns path prepended with directory", {
  skip_if_offline()
  expect_identical(
    check_path("deployments_schema.json", directory = "data"),
    "data/deployments_schema.json"
  )

  # No prepending with default directory (NULL)
  expect_identical(
    check_path("data/deployments_schema.json"),
    "data/deployments_schema.json"
  )

  # Directory is ignored for URL
  url <- file.path(
    "https://raw.githubusercontent.com/frictionlessdata/frictionless-r/",
    "main/tests/testthat/data/deployments_schema.json"
  )
  expect_identical(check_path(url, directory = "data"), url)
})

test_that("check_path() returns error on absolute path when safe = TRUE", {
  expect_error(
    check_path("/dir/file.txt", safe = TRUE),
    class = "frictionless_error_path_unsafe_absolute"
  )
  expect_error(
    frictionless:::check_path("/dir/file.txt", safe = TRUE),
    regexp = "`path` must be a safe path.",
    fixed = TRUE
  )
  expect_error(
    frictionless:::check_path("/dir/file.txt", safe = TRUE),
    regexp = paste(
      "'/dir/file.txt' is an absolute path starting with \"/\" which is unsafe."
    ),
    fixed = TRUE
  )
})

test_that("check_path() returns error on relative parent path when safe =
           TRUE", {
  expect_error(
    check_path("../dir/file.txt", safe = TRUE),
    class = "frictionless_error_path_unsafe_relative"
  )
  expect_error(
    check_path("../dir/file.txt", safe = TRUE),
    regexp = "`path` must be a safe path.",
    fixed = TRUE
  )
  expect_error(
    check_path("../dir/file.txt", safe = TRUE),
    regexp = paste(
      "'../dir/file.txt' is a relative parent path starting with \"../\" which",
      "is unsafe."
    ),
    fixed = TRUE
  )
})

test_that("check_path() returns error when local file cannot be found", {
  expect_error(
    check_path("no_such_file.csv"),
    class = "frictionless_error_path_not_found"
  )
  expect_error(
    check_path("no_such_file.csv"),
    regexp = "Can't find file at 'no_such_file.csv'.",
    fixed = TRUE
  )
})

test_that("check_path() returns error when remote file cannot be found", {
  skip_if_offline()
  expect_error(
    check_path("https://example.com/no_such_file.csv"),
    class = "frictionless_error_url_not_found"
  )
  expect_error(
    check_path("https://example.com/no_such_file.csv"),
    regexp = "Can't find file at <https://example.com/no_such_file.csv>.",
    fixed = TRUE
  )
})
