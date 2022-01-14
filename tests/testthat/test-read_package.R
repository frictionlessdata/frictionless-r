test_that("read_package() returns a valid Data Package, whether reading path or
           url", {
  # Load example package (locally and remotely) and a valid minimal one
  p_path <- system.file("extdata", "datapackage.json", package = "frictionless")
  p_url <- file.path("https://raw.githubusercontent.com/frictionlessdata/",
                       "frictionless-r/main/inst/extdata/datapackage.json")
  minimal_path <- "data/valid_minimal.json"
  p_local <- suppressMessages(read_package(p_path))
  p_remote <- suppressMessages(read_package(p_url))
  p_minimal <- suppressMessages(read_package(minimal_path))

  # Returns a list of class "datapackage", with at minimal resources,
  # resource_names and directory
  expect_true(check_package(p_local))
  expect_true(check_package(p_remote))
  expect_true(check_package(p_minimal))

  # Package has correct "resource_names"
  resource_names <- c("deployments", "observations", "media")
  expect_identical(p_local$resource_names, resource_names)
  expect_identical(p_remote$resource_names, resource_names)
  expect_identical(p_minimal$resource_names, resource_names)

  # Package has correct "directory", containing root dir of datapackage.json
  expect_identical(p_local$directory, gsub("/datapackage.json", "", p_path))
  expect_identical(p_remote$directory, gsub("/datapackage.json", "", p_url))
  expect_identical(p_minimal$directory, "data")
})

test_that("read_package() shows message about usage norms", {
  # Load example package and a minimal valid one a URL in "id"
  p_path <- system.file("extdata", "datapackage.json", package = "frictionless")
  minimal_extra_path <- "data/valid_minimal_extra.json"

  expected_message <- glue::glue(
    "Please make sure you have the right to access data from this",
    "Data Package for your intended use.\n",
    "Follow applicable norms or requirements to credit the dataset",
    "and its authors.",
    .sep = " "
  )
  expect_message(
    read_package(p_path),
    expected_message,
    fixed = TRUE
  )
  expect_message(
    read_package(minimal_extra_path),
    paste(
      expected_message,
      "For more information, see https://example.com",
      sep = "\n"
    ),
    fixed = TRUE
  )
})

test_that("read_package() returns error on missing file and properties", {
  # Incorrect type
  expect_error(
    read_package(list()),
    "`file` must be a path or URL to a `datapackage.json` file.",
    fixed = TRUE
  )

  # No file
  expect_error(
    read_package("nofile.json"),
    "Can't find file at `nofile.json`",
    fixed = TRUE
  )
  expect_error(
    read_package("http://example.com/nofile.json"),
    "Can't find file at `http://example.com/nofile.json`.",
    fixed = TRUE
  )

  # Not a json file
  expect_error(
    read_package(
      system.file("extdata", "deployments.csv", package = "frictionless")
    ),
    "lexical error: invalid char in json text."
  )

  # No resources
  expect_error(
    read_package("data/resources_missing.json"),
    paste(
      "`data/resources_missing.json` must have property `resources`",
      "containing at least one resource. All resources must have a `name`."
    ),
    fixed = TRUE
  )

  # Empty resources
  expect_error(
    read_package("data/resources_empty.json"),
    paste(
      "`data/resources_empty.json` must have property `resources`",
      "containing at least one resource. All resources must have a `name`."
    ),
    fixed = TRUE
  )

  # No resource name
  expect_error(
    read_package("data/resources_no_name.json"),
    paste(
      "`data/resources_no_name.json` must have property `resources`",
      "containing at least one resource. All resources must have a `name`."
    ),
    fixed = TRUE
  )
})

test_that("read_package() allows descriptor at absolute or relative parent
           path", {
  relative_path <- "../testthat/data/valid_minimal.json"
  expect_true(
    check_package(suppressMessages(read_package(relative_path)))
  )
  absolute_path <- normalizePath("data/valid_minimal.json")
  expect_true(
    check_package(suppressMessages(read_package(absolute_path)))
  )
})

test_that("read_package() allows YAML descriptor", {
  expect_true(
    check_package(suppressMessages(read_package("data/valid_minimal.yml")))
  )
})
