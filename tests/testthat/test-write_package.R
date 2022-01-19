test_that("write_package() returns output Data Package (invisibly)", {
  p <- example_package
  # Note write_package() is expected to create directory without warning
  dir <- file.path(tempdir(), "package")
  on.exit(unlink(dir, recursive = TRUE))
  p_written <- suppressMessages(write_package(p, dir))
  p_from_file <- suppressMessages(read_package(
    file.path(dir, "datapackage.json")
  ))
  # p_from_file$directory will differ: overwrite to make the same
  p_from_file$directory <- p_written$directory

  expect_invisible(suppressMessages(write_package(p, dir)))
  expect_identical(p_written, p_from_file)
})

test_that("write_package() returns error on incorrect Data Package", {
  expect_error(
    write_package(list()),
    paste(
      "`package` must be a list object created with `read_package()` or",
      "`create_package()`."
    ),
    fixed = TRUE
  )
})

test_that("write_package() returns error if Data Package has no resource(s)", {
  p_empty <- create_package()
  dir <- file.path(tempdir(), "package")
  on.exit(unlink(dir, recursive = TRUE))
  expect_error(
    write_package(p_empty, dir),
    "`package` must have resources. Use `add_resource()` to add resources.",
    fixed = TRUE
  )

  # Resources without name are tested in test-check_package.R
  # Resources without path or data are tested in test-read_resource.R
})

test_that("write_package() writes unaltered datapackage.json as is", {
  p <- suppressMessages(read_package(
    system.file("extdata", "datapackage.json", package = "frictionless")
  ))
  json_original <- readr::read_file(
    system.file("extdata", "datapackage.json", package = "frictionless")
  )
  dir <- file.path(tempdir(), "package")
  on.exit(unlink(dir, recursive = TRUE))
  suppressMessages(write_package(p, dir))
  json_as_written <- readr::read_file(file.path(dir, "datapackage.json"))

  # Output json = input json. This also tests if new properties (resource_names,
  # directories) are removed and json is printed "pretty"
  expect_identical(json_as_written, json_original)
})

test_that("write_package() copies file(s) for path = local in local package", {
  p <- suppressMessages(read_package(
    system.file("extdata", "datapackage.json", package = "frictionless")
  ))
  p$resources[[2]]$path[[2]] <- "observations_2.csv" # Make one URL a local path
  p <- add_resource(p, "new", "data/df.csv")
  dir <- file.path(tempdir(), "package")
  on.exit(unlink(dir, recursive = TRUE))
  p_written <- suppressMessages(write_package(p, dir))

  # Original resource "deployments" with local path
  expect_identical(p_written$resources[[1]]$path, "deployments.csv")
  expect_true(file.exists(file.path(dir, "deployments.csv")))

  # Original resource "observations" with URL + local path => both local
  expect_identical(
    p_written$resources[[2]]$path,
    c("observations_1.csv", "observations_2.csv")
  )
  expect_true(file.exists(file.path(dir, "observations_1.csv")))
  expect_true(file.exists(file.path(dir, "observations_2.csv")))

  # New resource "new" with local path
  expect_identical(p_written$resources[[4]]$path, "df.csv") # Keeps file name
  expect_true(file.exists(file.path(dir, "df.csv")))
})

test_that("write_package() downloads file(s) for path = local in remote
           package", {
  p <- example_package
  p$resources[[2]]$path[[2]] <- "observations_2.csv" # Make one URL a local path
  p <- add_resource(p, "new", "data/df.csv")
  dir <- file.path(tempdir(), "package")
  on.exit(unlink(dir, recursive = TRUE))
  p_written <- suppressMessages(write_package(p, dir))

  # Original resource "deployments" with local path
  expect_identical(p_written$resources[[1]]$path, "deployments.csv")
  expect_true(file.exists(file.path(dir, "deployments.csv")))

  # Original resource "observations" with URL + local path => both local
  expect_identical(
    p_written$resources[[2]]$path,
    c("observations_1.csv", "observations_2.csv")
  )
  expect_true(file.exists(file.path(dir, "observations_1.csv")))
  expect_true(file.exists(file.path(dir, "observations_2.csv")))

  # New resource "new" with local path
  expect_identical(p_written$resources[[4]]$path, "df.csv") # Keeps file name
  expect_true(file.exists(file.path(dir, "df.csv")))
})

test_that("write_package() leaves as is for path = URL in local package", {
  p <- suppressMessages(read_package(
    system.file("extdata", "datapackage.json", package = "frictionless")
  ))
  p <- add_resource(p, "new", file.path(
    "https://raw.githubusercontent.com/frictionlessdata/frictionless-r",
    "main/tests/testthat/data/df.csv"
  ))
  dir <- file.path(tempdir(), "package")
  on.exit(unlink(dir, recursive = TRUE))
  p_written <- suppressMessages(write_package(p, dir))

  # Original resource "observations" where all paths are URLs
  expect_identical(p_written$resources[[2]]$path, p$resources[[2]]$path)
  expect_false(file.exists(file.path(dir, "observations_1.csv")))
  expect_false(file.exists(file.path(dir, "observations_2.csv")))

  # New resource "new" with URL path
  expect_identical(p_written$resources[[4]]$path, p$resources[[4]]$path)
  expect_false(file.exists(file.path(dir, "df.csv")))
})

test_that("write_package() leaves as is for path = URL in remote package", {
  p <- example_package
  p <- add_resource(p, "new", file.path(
    "https://raw.githubusercontent.com/frictionlessdata/frictionless-r",
    "main/tests/testthat/data/df.csv"
  ))
  dir <- file.path(tempdir(), "package")
  on.exit(unlink(dir, recursive = TRUE))
  p_written <- suppressMessages(write_package(p, dir))

  # Original resource "observations" where all paths are URLs
  expect_identical(p_written$resources[[2]]$path, p$resources[[2]]$path)
  expect_false(file.exists(file.path(dir, "observations_1.csv")))
  expect_false(file.exists(file.path(dir, "observations_2.csv")))

  # New resource "new" with URL path
  expect_identical(p_written$resources[[4]]$path, p$resources[[4]]$path)
  expect_false(file.exists(file.path(dir, "df.csv")))
})

test_that("write_package() leaves as is for data = json in local package", {
  p <- suppressMessages(read_package(
    system.file("extdata", "datapackage.json", package = "frictionless")
  ))
  dir <- file.path(tempdir(), "package")
  on.exit(unlink(dir, recursive = TRUE))
  p_written <- suppressMessages(write_package(p, dir))

  # Original resource "media" with data = json
  expect_identical(p_written$resources[[3]]$data, p$resources[[3]]$data)
  expect_false(file.exists(file.path(dir, "media.csv")))

  # New resources cannot have data = json
})

test_that("write_package() leaves as is for data = json in remote package", {
  p <- example_package
  dir <- file.path(tempdir(), "package")
  on.exit(unlink(dir, recursive = TRUE))
  p_written <- suppressMessages(write_package(p, dir))

  # New resource "media" with data = json
  expect_identical(p_written$resources[[3]]$data, p$resources[[3]]$data)
  expect_false(file.exists(file.path(dir, "media.csv")))

  # New resources cannot have data = json
})

test_that("write_package() creates file for data = df in local package", {
  p <- suppressMessages(read_package(
    system.file("extdata", "datapackage.json", package = "frictionless")
  ))
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  p <- add_resource(p, "new", df)
  dir <- file.path(tempdir(), "package")
  on.exit(unlink(dir, recursive = TRUE))
  p_written <- suppressMessages(write_package(p, dir))

  # Original resources cannot have data = df

  # New resource "new" with data = df
  expect_identical(p_written$resources[[4]]$path, "new.csv")
  expect_null(p_written$resources[[4]]$data)
  expect_true(file.exists(file.path(dir, "new.csv")))
})

test_that("write_package() creates file for data = df in remote package", {
  p <- example_package
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  p <- add_resource(p, "new", df)
  dir <- file.path(tempdir(), "package")
  on.exit(unlink(dir, recursive = TRUE))
  p_written <- suppressMessages(write_package(p, dir))

  # Original resources cannot have data = df

  # New resource "new" with data = df
  expect_identical(p_written$resources[[4]]$path, "new.csv")
  expect_null(p_written$resources[[4]]$data)
  expect_true(file.exists(file.path(dir, "new.csv")))
})

test_that("write_package() shows message when downloading file", {
  p <- example_package
  dir <- file.path(tempdir(), "package")
  on.exit(unlink(dir, recursive = TRUE))
  expect_message(
    write_package(p, dir),
    paste0(
      "Downloading file from https://raw.githubusercontent.com/",
      "frictionlessdata/frictionless-r/main/inst/extdata/deployments.csv"
    ),
    fixed = TRUE
  )
})

test_that("write_package() sets correct properties for data frame resources", {
  p <- example_package
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  schema <- create_schema(df)
  p <- add_resource(p, "new", df)
  dir <- file.path(tempdir(), "package")
  on.exit(unlink(dir, recursive = TRUE))
  p_written <- suppressMessages(write_package(p, dir))
  resource_written <- p_written$resources[[4]]

  # Added resource has correct properties
  expect_identical(resource_written$name, "new")
  expect_identical(resource_written$path, "new.csv")
  expect_identical(resource_written$profile, "tabular-data-resource")
  expect_null(resource_written$title)
  expect_null(resource_written$description)
  expect_identical(resource_written$format, "csv")
  expect_identical(resource_written$mediatype, "text/csv")
  expect_identical(resource_written$encoding, "utf-8")
  expect_null(resource_written$dialect)
  expect_null(resource_written$bytes)
  expect_null(resource_written$hash)
  expect_null(resource_written$sources)
  expect_null(resource_written$licenses)
  expect_identical(resource_written$schema, schema)
  expect_null(resource_written$data)
  expect_null(resource_written$read_from)
})
