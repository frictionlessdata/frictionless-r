test_that("write_package() returns a input Data Package (invisibly)", {
  pkg <- example_package
  temp_dir <- tempdir()
  on.exit(unlink(temp_dir, recursive = TRUE))
  expect_invisible(write_package(pkg, temp_dir))
  pkg_out <- write_package(pkg, temp_dir)
  expect_identical(pkg_out, pkg)
})

test_that("write_package() returns error on incorrect Data Package", {
  expect_error(
    write_package(list()),
    paste(
      "`package` must be a list object of class `datapackage` created with",
      "`read_package()` or `create_package()`."
    ),
    fixed = TRUE
  )
})

test_that("write_package() returns error if Data Package has no resource(s)", {
  pkg_empty <- create_package()
  temp_dir <- tempdir()
  on.exit(unlink(temp_dir, recursive = TRUE))
  expect_error(
    write_package(pkg_empty, temp_dir),
    "`package` must have resources. Use `add_resource()` to add resources.",
    fixed = TRUE
  )

  # Resources without name are tested in test-check_package.R
  # Resources without path or data are tested in test-read_resource.R
})

test_that("write_package() writes to the specified directory", {
  pkg <- example_package
  temp_subdir <- file.path(tempdir(), "x/y")
  on.exit(unlink(temp_subdir, recursive = TRUE))

  # Function should create subdir(s) without error
  expect_invisible(write_package(pkg, temp_subdir))

  # Valid package can be found at temp_subdir
  expect_true(check_package(
    suppressMessages(read_package(file.path(temp_subdir, "datapackage.json")))
  ))
})

test_that("write_package() writes unaltered datapackage.json as is", {
  pkg <- suppressMessages(read_package(
    system.file("extdata", "datapackage.json", package = "frictionless")
  ))
  json_in <- readr::read_file(
    system.file("extdata", "datapackage.json", package = "frictionless")
  )
  temp_dir <- tempdir()
  on.exit(unlink(temp_dir, recursive = TRUE))
  write_package(pkg, temp_dir)
  json_out <- readr::read_file(file.path(temp_dir, "datapackage.json"))

  # Output json = input json. This also tests if new properties (resource_names,
  # directories) are removed and json is printed "pretty"
  expect_identical(json_out, json_in)
})

test_that("write_package() leaves resources with URL as is, but updates path to
           URLs", {
  pkg <- example_package # Example Data Package is a remote one
  temp_dir <- tempdir()
  on.exit(unlink(temp_dir, recursive = TRUE))
  write_package(pkg, temp_dir)
  pkg_out <- suppressMessages(read_package(
    file.path(temp_dir, "datapackage.json")
  ))

  # Resources are unchanged, except that local paths are now URLs
  pkg$resources[[1]]$path <- file.path(
    "https://raw.githubusercontent.com/frictionlessdata",
    "frictionless-r/main/inst/extdata/deployments.csv"
  )
  pkg$resources[[2]]$path <- c(
    file.path("https://raw.githubusercontent.com/frictionlessdata",
              "frictionless-r/main/inst/extdata/observations_1.csv"),
    file.path("https://raw.githubusercontent.com/frictionlessdata",
              "frictionless-r/main/inst/extdata/observations_2.csv")
  )
  expect_identical(pkg_out$resources[[1]], pkg$resources[[1]])
  expect_identical(pkg_out$resources[[2]], pkg$resources[[2]])

  # Do not expect files
  expect_error(
    readr::read_file(file.path(temp_dir, "deployments.csv")),
    "'.*deployments.csv' does not exist."
    # no fixed = TRUE, since full returned path depends on system
  )
  expect_error(
    readr::read_file(file.path(temp_dir, "observations_1.csv")),
    "'.*observations_1.csv' does not exist."
    # no fixed = TRUE, since full returned path depends on system
  )
  expect_error(
    readr::read_file(file.path(temp_dir, "observations_2.csv")),
    "'.*observations_2.csv' does not exist."
    # no fixed = TRUE, since full returned path depends on system
  )
})

test_that("write_package() leaves resources with path as is, but copies
           files", {
  pkg <- suppressMessages(read_package(
    system.file("extdata", "datapackage.json", package = "frictionless")
  ))
  temp_dir <- tempdir()
  on.exit(unlink(temp_dir, recursive = TRUE))
  write_package(pkg, temp_dir)
  pkg_out <- suppressMessages(read_package(
    file.path(temp_dir, "datapackage.json")
  ))

  # Resources are unchanged
  expect_identical(pkg_out$resources[[1]], pkg$resources[[1]])
  expect_identical(pkg_out$resources[[2]], pkg$resources[[2]])

  # Files are written
  expect_type(
    readr::read_file(file.path(temp_dir, "deployments.csv")), "character"
  )
  expect_type(
    readr::read_file(file.path(temp_dir, "observations_1.csv")), "character"
  )
  expect_type(
    readr::read_file(file.path(temp_dir, "observations_2.csv")), "character"
  )
})

test_that("write_package() leaves existing resources with `data` as is", {
  pkg <- example_package
  temp_dir <- tempdir()
  write_package(pkg, temp_dir)
  pkg_out <- suppressMessages(read_package(
    file.path(temp_dir, "datapackage.json")
  ))

  # Resource is unchanged
  expect_identical(pkg_out$resources[[3]], pkg$resources[[3]])
})

test_that("write_package() creates files for new resources", {
  pkg <- example_package
  df <- data.frame(
    "col_1" = c(1, 2),
    "col_2" = factor(c("a", "b"), levels = c("a", "b", "c"))
  )
  pkg <- add_resource(pkg, "new", df)
  temp_dir <- tempdir()
  on.exit(unlink(temp_dir, recursive = TRUE))
  write_package(pkg, temp_dir)
  pkg_out <- suppressMessages(read_package(
    file.path(temp_dir, "datapackage.json")
  ))

  # Added resource has path (not data) and was written to file
  expect_identical(pkg_out$resources[[4]]$path, "new.csv")
  expect_null(pkg_out$resources[[4]]$data)
  expect_type(readr::read_file(file.path(temp_dir, "new.csv")), "character")
})

test_that("write_package() adds correct properties for new resources", {
  pkg <- example_package
  df <- data.frame(
    "col_1" = c(1, 2),
    "col_2" = factor(c("a", "b"), levels = c("a", "b", "c"))
  )
  schema <- create_schema(df)
  pkg <- add_resource(pkg, "new", df)
  temp_dir <- tempdir()
  on.exit(unlink(temp_dir, recursive = TRUE))
  write_package(pkg, temp_dir)
  pkg_out <- suppressMessages(read_package(
    file.path(temp_dir, "datapackage.json")
  ))
  resource_out <- pkg_out$resources[[4]]

  # Added resource has correct properties
  expect_identical(resource_out$name, "new")
  expect_identical(resource_out$path, "new.csv")
  expect_identical(resource_out$profile, "tabular-data-resource")
  expect_null(resource_out$title)
  expect_null(resource_out$description)
  expect_identical(resource_out$format, "csv")
  expect_identical(resource_out$mediatype, "text/csv")
  expect_identical(resource_out$encoding, "utf-8")
  expect_null(resource_out$dialect)
  expect_null(resource_out$bytes)
  expect_null(resource_out$hash)
  expect_null(resource_out$sources)
  expect_null(resource_out$licenses)
  expect_identical(resource_out$schema, schema)
  expect_null(resource_out$data)
  expect_null(resource_out$read_from)
})
