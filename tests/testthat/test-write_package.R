test_that("write_package() returns output Data Package (invisibly)", {
  p <- example_package()

  # Note write_package() is expected to create directory without warning
  dir <- file.path(tempdir(), "package")
  on.exit(unlink(dir, recursive = TRUE))
  p_written <- suppressMessages(write_package(p, dir))
  p_from_file <- read_package(file.path(dir, "datapackage.json"))

  # p_from_file$directory will differ: overwrite to make the same
  p_from_file$directory <- p_written$directory

  expect_invisible(suppressMessages(write_package(p, dir)))
  expect_identical(p_written, p_from_file)
})

test_that("write_package() returns error on invalid Data Package", {
  expect_error(
    write_package(list()),
    class = "frictionless_error_package_invalid"
  )
})

test_that("write_package() returns error if Data Package has no resource(s)", {
  p_empty <- create_package()
  dir <- file.path(tempdir(), "package")
  on.exit(unlink(dir, recursive = TRUE))
  expect_error(
    write_package(p_empty, dir),
    class = "frictionless_error_package_without_resources"
  )
  expect_error(
    write_package(p_empty, dir),
    regexp = "`package` must have resources.",
    fixed = TRUE
  )
  expect_error(
    write_package(p_empty, dir),
    regexp = "Use `add_resource()` to add resources.",
    fixed = TRUE
  )

  # Resources without name are tested in test-check_package.R
  # Resources without path or data are tested in test-read_resource.R
})

test_that("write_package() writes unaltered datapackage.json as is", {
  p_file <-
    system.file("extdata", "v1", "datapackage.json", package = "frictionless")
  json_original <- readr::read_lines(p_file) # Will use line endings of system
  p <- read_package(p_file)
  dir <- file.path(tempdir(), "package")
  on.exit(unlink(dir, recursive = TRUE))
  suppressMessages(write_package(p, dir))
  json_as_written <- readr::read_lines(file.path(dir, "datapackage.json"))

  # Output json = input json. This also tests if custom property "directory"
  # is removed and json is printed "pretty"
  expect_identical(json_as_written, json_original)
})

test_that("write_package() does not overwrite existing data files", {
  skip_if_offline()
  p <- example_package()

  # Change local path to URL
  p$resources[[1]]$path <- file.path(
    "https://raw.githubusercontent.com/frictionlessdata/frictionless-r",
    "main/inst/extdata/v1/deployments.csv"
  )
  dir <- file.path(tempdir(), "package")
  on.exit(unlink(dir, recursive = TRUE))
  dir.create(dir)

  # Create files in directory
  files <- c(
    datapackage = file.path(dir, "datapackage.json"),
    # deployments: has remote path, should not be written
    observations_1 = file.path(dir, "observations_1.tsv"),
    observations_2 = file.path(dir, "observations_2.tsv")
  )
  file.create(files) # Size for these files will be 0

  # Write package to directory, expect only datapackage.json is overwritten
  suppressMessages(write_package(p, dir))
  expect_gt(file.info(files["datapackage"])$size, 0) # Overwritten
  expect_identical(file.info(files["observations_1"])$size, 0) # Remains same
})

test_that("write_package() copies file(s) for path = local in local package", {
  skip_if_offline()
  p <- example_package()

  # Change one local path to URL
  p$resources[[2]]$path[[1]] <- file.path(
    "https://raw.githubusercontent.com/frictionlessdata/frictionless-r",
    "main/inst/extdata/v1/observations_1.tsv"
  )
  p <- add_resource(p, "new", test_path("data/df.csv"))
  dir <- file.path(tempdir(), "package")
  on.exit(unlink(dir, recursive = TRUE))
  p_written <- suppressMessages(write_package(p, dir))

  # Original resource "deployments" with local path
  expect_identical(p_written$resources[[1]]$path, "deployments.csv")
  expect_true(file.exists(file.path(dir, "deployments.csv")))

  # Original resource "observations" with URL + local path => both local
  expect_identical(
    p_written$resources[[2]]$path,
    c("observations_1.tsv", "observations_2.tsv")
  )
  expect_true(file.exists(file.path(dir, "observations_1.tsv")))
  expect_true(file.exists(file.path(dir, "observations_2.tsv")))

  # New resource "new" with local path
  expect_identical(p_written$resources[[4]]$path, "df.csv") # Keeps file name
  expect_true(file.exists(file.path(dir, "df.csv")))
})

test_that("write_package() downloads file(s) for path = local in remote
           package", {
  skip_if_offline()
  p <- example_package()

  # Make remote
  p$directory <- file.path(
    "https://raw.githubusercontent.com/frictionlessdata/frictionless-r/",
    "main/inst/extdata/v1"
  )

  # Change one local path to URL
  p$resources[[2]]$path[[1]] <- file.path(
    "https://raw.githubusercontent.com/frictionlessdata/frictionless-r",
    "main/inst/extdata/v1/observations_1.tsv"
  )
  p <- add_resource(p, "new", test_path("data/df.csv"))
  dir <- file.path(tempdir(), "package")
  on.exit(unlink(dir, recursive = TRUE))
  p_written <- suppressMessages(write_package(p, dir))

  # Original resource "deployments" with local path
  expect_identical(p_written$resources[[1]]$path, "deployments.csv")
  expect_true(file.exists(file.path(dir, "deployments.csv")))

  # Original resource "observations" with URL + local path => both local
  expect_identical(
    p_written$resources[[2]]$path,
    c("observations_1.tsv", "observations_2.tsv")
  )
  expect_true(file.exists(file.path(dir, "observations_1.tsv")))
  expect_true(file.exists(file.path(dir, "observations_2.tsv")))

  # New resource "new" with local path
  expect_identical(p_written$resources[[4]]$path, "df.csv") # Keeps file name
  expect_true(file.exists(file.path(dir, "df.csv")))
})

test_that("write_package() leaves as is for path = URL in local package", {
  skip_if_offline()
  p <- example_package()

  # Change local path to URL
  p$resources[[1]]$path <- file.path(
    "https://raw.githubusercontent.com/frictionlessdata/frictionless-r",
    "main/inst/extdata/v1/deployments.csv"
  )
  p <- add_resource(p, "new", file.path(
    "https://raw.githubusercontent.com/frictionlessdata/frictionless-r",
    "main/tests/testthat/data/df.csv"
  ))
  dir <- file.path(tempdir(), "package")
  on.exit(unlink(dir, recursive = TRUE))
  p_written <- suppressMessages(write_package(p, dir))

  # Original resource "deployments" with URL path
  expect_identical(p_written$resources[[1]]$path, p$resources[[1]]$path)
  expect_false(file.exists(file.path(dir, "deployments.csv")))

  # New resource "new" with URL path
  expect_identical(p_written$resources[[4]]$path, p$resources[[4]]$path)
  expect_false(file.exists(file.path(dir, "df.csv")))
})

test_that("write_package() leaves as is for path = URL in remote package", {
  skip_if_offline()
  p <- example_package()

  # Make remote
  p$directory <- file.path(
    "https://raw.githubusercontent.com/frictionlessdata/frictionless-r/",
    "main/inst/extdata/v1"
  )

  # Change local path to URL
  p$resources[[1]]$path <- file.path(
    "https://raw.githubusercontent.com/frictionlessdata/frictionless-r",
    "main/inst/extdata/v1/deployments.csv"
  )
  p <- add_resource(p, "new", file.path(
    "https://raw.githubusercontent.com/frictionlessdata/frictionless-r",
    "main/tests/testthat/data/df.csv"
  ))
  dir <- file.path(tempdir(), "package")
  on.exit(unlink(dir, recursive = TRUE))
  p_written <- suppressMessages(write_package(p, dir))

  # Original resource "observations" where all paths are URLs
  expect_identical(p_written$resources[[1]]$path, p$resources[[1]]$path)
  expect_false(file.exists(file.path(dir, "deployments.csv")))

  # New resource "new" with URL path
  expect_identical(p_written$resources[[4]]$path, p$resources[[4]]$path)
  expect_false(file.exists(file.path(dir, "df.csv")))
})

test_that("write_package() leaves as is for data = json in local package", {
  p <- example_package()
  dir <- file.path(tempdir(), "package")
  on.exit(unlink(dir, recursive = TRUE))
  p_written <- suppressMessages(write_package(p, dir))

  # Original resource "media" with data = json
  expect_identical(p_written$resources[[3]]$data, p$resources[[3]]$data)
  expect_false(file.exists(file.path(dir, "media.csv")))

  # New resources cannot have data = json
})

test_that("write_package() leaves as is for data = json in remote package", {
  skip_if_offline()
  p <- example_package()

  # Make remote
  p$directory <- file.path(
    "https://raw.githubusercontent.com/frictionlessdata/frictionless-r/",
    "main/inst/extdata/v1"
  )
  dir <- file.path(tempdir(), "package")
  on.exit(unlink(dir, recursive = TRUE))
  p_written <- suppressMessages(write_package(p, dir))

  # New resource "media" with data = json
  expect_identical(p_written$resources[[3]]$data, p$resources[[3]]$data)
  expect_false(file.exists(file.path(dir, "media.csv")))

  # New resources cannot have data = json
})

test_that("write_package() creates file for data = df in local package", {
  p <- example_package()
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
  skip_if_offline()
  p <- example_package()

  # Make remote
  p$directory <- file.path(
    "https://raw.githubusercontent.com/frictionlessdata/frictionless-r/",
    "main/inst/extdata/v1"
  )
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
  skip_if_offline()
  p <- example_package()

  # Change one local path to URL
  p$resources[[2]]$path[[1]] <- file.path(
    "https://raw.githubusercontent.com/frictionlessdata/frictionless-r",
    "main/inst/extdata/v1/observations_1.tsv"
  )
  dir <- file.path(tempdir(), "package")
  dir_1 <- file.path(dir, "1")
  dir_2 <- file.path(dir, "2")
  on.exit(unlink(dir, recursive = TRUE))
  expect_message(
    write_package(p, dir_1),
    class = "frictionless_message_file_downloading"
  )
  expect_message(
    write_package(p, dir_2),
    regexp = paste0(
      "Downloading file from 'https://raw.githubusercontent.com/",
      "frictionlessdata/frictionless-r/main/inst/v1/extdata/observations_1.tsv'"
    ),
    fixed = TRUE
  )
})

test_that("write_package() sets correct properties for data frame resources", {
  p <- example_package()
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
  expect_identical(resource_written$format, "csv")
  expect_identical(resource_written$mediatype, "text/csv")
  expect_identical(resource_written$encoding, "utf-8")
  expect_null(resource_written$dialect)
  expect_identical(resource_written$schema, schema)
  expect_null(resource_written$data)
  expect_null(resource_written$read_from)
})

test_that("write_package() retains custom properties set in add_resource()", {
  p <- create_package()
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  p <- add_resource(p, "new_df", df, title = "custom_title", foo = "bar")
  df_csv <- test_path("data/df.csv")
  p <- add_resource(p, "new_csv", df_csv, title = "custom_title", foo = "bar")
  dir <- file.path(tempdir(), "package")
  on.exit(unlink(dir, recursive = TRUE))
  p_written <- suppressMessages(write_package(p, dir))

  expect_identical(p_written$resources[[1]]$title, "custom_title")
  expect_identical(p_written$resources[[1]]$foo, "bar")
  expect_identical(p_written$resources[[2]]$title, "custom_title")
  expect_identical(p_written$resources[[2]]$foo, "bar")
})

test_that("write_package() will gzip file for compress = TRUE", {
  p <- example_package()
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  p <- add_resource(p, "new", df)
  dir <- file.path(tempdir(), "package")
  on.exit(unlink(dir, recursive = TRUE))
  p_written <- suppressMessages(write_package(p, dir, compress = TRUE))
  resource_written <- p_written$resources[[4]]

  # Writes correct file to disk
  expect_identical(resource_written$path, "new.csv.gz")
  expect_true(file.exists(file.path(dir, "new.csv.gz")))
  expect_false(file.exists(file.path(dir, "new.csv")))

  # Written file can be read by read_resource()
  p_reread <- read_package(file.path(dir, "datapackage.json"))
  expect_identical(read_resource(p_reread, "new"), dplyr::as_tibble(df))
})

test_that("write_package() writes NULL and NA as null", {
  p <- example_package()
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))

  # Set some properties to NULL and NA (p$image is already read as NULL)
  p$null_property <- NULL
  p$na_property <- NA
  p <- add_resource(p, "new", df, na_property = NA)

  # Write and read package
  dir <- file.path(tempdir(), "package")
  on.exit(unlink(dir, recursive = TRUE))
  p_written <- suppressMessages(write_package(p, dir, compress = TRUE))
  p_reread <- read_package(file.path(dir, "datapackage.json"))

  # Properties are written as NULL (use chuck() to force error if missing)
  expect_null(purrr::chuck(p_reread, "image"))
  expect_null(p_reread$null_property) # Write should remove this property
  expect_null(purrr::chuck(p_reread, "na_property"))
  expect_null(purrr::chuck(p_reread, "resources", 4, "na_property"))
})
