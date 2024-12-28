test_that("read_package() returns a valid Data Package reading from path", {
  # Load example package and a valid minimal one
  p_path <- system.file("extdata", "v1", "datapackage.json", package = "frictionless")
  minimal_path <- test_path("data/valid_minimal.json")
  p_local <- read_package(p_path)
  p_minimal <- read_package(minimal_path)

  # Returns a list with required properties
  expect_no_error(check_package(p_local))
  expect_no_error(check_package(p_minimal))

  # Package has correct resources
  resource_names <- c("deployments", "observations", "media")
  expect_identical(resources(p_local), resource_names)
  expect_identical(resources(p_minimal), resource_names)

  # Package has correct "directory", containing root dir of datapackage.json
  expect_identical(
    p_local$directory,
    sub("/datapackage.json", "", p_path, fixed = TRUE)
  )
  expect_identical(p_minimal$directory, "data")
})

test_that("read_package() returns a valid Data Package reading from url", {
  skip_if_offline()
  # Load example package remotely
  p_url <- file.path(
    "https://raw.githubusercontent.com/frictionlessdata/frictionless-r/",
    "main/inst/extdata/v1/datapackage.json"
  )
  p_remote <- read_package(p_url)

  # Returns a list with required properties
  expect_no_error(check_package(p_remote))

  # Package has correct resources
  resource_names <- c("deployments", "observations", "media")
  expect_identical(resources(p_remote), resource_names)

  # Package has correct "directory", containing root dir of datapackage.json
  expect_identical(
    p_remote$directory,
    sub("/datapackage.json", "", p_url, fixed = TRUE)
  )
})

test_that("read_package() returns error on missing or invalid file", {
  skip_if_offline()
  # Incorrect type
  expect_error(
    read_package(list()),
    class = "frictionless_error_file_invalid"
  )
  expect_error(
    read_package(list()),
    regexp = "`file` must be a path or URL to a 'datapackage.json' file.",
    fixed = TRUE
  )

  # No file locally
  expect_error(
    read_package("nofile.json"),
    class = "frictionless_error_path_not_found"
  )

  # Not a json file
  expect_error(
    read_package(
      system.file("extdata", "v1", "deployments.csv", package = "frictionless")
    ),
    regexp = "lexical error: invalid char in json text.",
    fixed = FALSE
  )

  # No file remotely
  expect_error(
    read_package("https://example.com/nofile.json"),
    class = "frictionless_error_url_not_found"
  )
})

test_that("read_package() warns if resources are missing", {
  # No resources property
  expect_warning(
    read_package(test_path("data/resources_missing.json")),
    class = "frictionless_warning_file_without_resources"
  )
  expect_warning(
    read_package(test_path("data/resources_missing.json")),
    regexp = paste(
      "`file` 'data/resources_missing.json' should have a resources property",
      "containing at least one resource."
    ),
    fixed = TRUE
  )
  expect_warning(
    read_package(test_path("data/resources_missing.json")),
    regexp = "Use `add_resource()` to add resources.",
    fixed = TRUE
  )

  # Resources is empty list
  expect_warning(
    read_package(test_path("data/resources_empty.json")),
    class = "frictionless_warning_file_without_resources"
  )
})

test_that("read_package() allows descriptor at absolute or relative parent
           path", {
  relative_path <- "../testthat/data/valid_minimal.json"
  expect_no_error(
    check_package(read_package(relative_path))
  )
  absolute_path <- normalizePath("data/valid_minimal.json")
  expect_no_error(
    check_package(read_package(absolute_path))
  )
})

test_that("read_package() allows YAML descriptor", {
  expect_no_error(
    check_package(read_package(test_path("data/valid_minimal.yml")))
  )
})

test_that("read_package() converts JSON null to NULL", {
  p_path <- system.file("extdata", "v1", "datapackage.json", package = "frictionless")
  p <- read_package(p_path)
  # { "image": null } is read as NULL (use chuck() to force error if missing)
  expect_null(purrr::chuck(p, "image"))
})


test_that("read_package() with `attach=TRUE`", {
  p_path <- system.file("extdata", "v1", "datapackage.json", package = "frictionless")
  p <- read_package(p_path, attach = TRUE)
  expect_s3_class(p$resources[[1]]$data, "data.frame")

  p_url <- file.path(
    "https://raw.githubusercontent.com/frictionlessdata/frictionless-r/",
    "main/inst/extdata/v1/datapackage.json"
  )
  p_remote <- read_package(p_url, attach = TRUE)
  expect_s3_class(p_remote$resources[[1]]$data, "data.frame")
})
