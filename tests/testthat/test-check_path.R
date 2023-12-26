test_that("check_path() returns path prepended with directory", {
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
  url <- file.path("https://github.com/frictionlessdata/frictionless-r/",
                   "raw/main/tests/testthat/data/deployments_schema.json")
  expect_identical(check_path(url, directory = "data"), url)
})

test_that("check_path() returns error on absolute path when safe = TRUE", {
  expect_error(
    check_path("/dir/file.txt", safe = TRUE),
    class = "frictionless_error_unsafe_absolute_path"
  )
  expect_error(
    frictionless:::check_path("/dir/file.txt", safe = TRUE),
    "`path` must be a safe path.",
    fixed = TRUE
  )
  expect_error(
    frictionless:::check_path("/dir/file.txt", safe = TRUE),
    "'/dir/file.txt' is an absolute path starting with \"/\" which is unsafe.",
    fixed = TRUE
  )
})

test_that("check_path() returns error on relative parent path when safe = TRUE", {
  expect_error(
    check_path("../dir/file.txt", safe = TRUE),
    class = "frictionless_error_unsafe_relative_path"
  )
  expect_error(
    check_path("../dir/file.txt", safe = TRUE),
    "`path` must be a safe path.",
    fixed = TRUE
  )
  expect_error(
    check_path("../dir/file.txt", safe = TRUE),
    "'../dir/file.txt' is a relative parent path starting with \"../\" which is unsafe.",
    fixed = TRUE
  )
})

test_that("check_path() returns error when local file cannot be found", {
  expect_error(
    check_path("no_such_file.csv"),
    class = "frictionless_error_not_found_path"
  )
  expect_error(
    check_path("no_such_file.csv"),
    "Can't find file at 'no_such_file.csv'.",
    fixed = TRUE
  )
})

test_that("check_path() returns error when remote file cannot be found", {
  expect_error(
    check_path("http://example.com/no_such_file.csv"),
    class = "frictionless_error_not_found_url"
  )
  expect_error(
    check_path("http://example.com/no_such_file.csv"),
    "Can't find file at <http://example.com/no_such_file.csv>.",
    fixed = TRUE
  )
})
