test_that("write_package() returns a input Data Package (invisibly)", {
  pkg <- suppressMessages(read_package(
    system.file("extdata", "datapackage.json", package = "frictionless")
  ))
  temp_dir <- tempdir()
  expect_invisible(write_package(pkg, temp_dir))
  pkg_out <- write_package(pkg, temp_dir)
  expect_equal(pkg, pkg_out)
  unlink(temp_dir, recursive = TRUE)
})

test_that("write_package() returns error on incorrect Data Package", {
  expect_error(
    write_package(list()),
    "`package` must be a list object of class `datapackage`"
  )
})

test_that("write_package() returns error if Data Package has no Data Resource(s)", {
  pkg_empty <- create_package()
  temp_dir <- tempdir()
  expect_error(
    write_package(pkg_empty, temp_dir),
    "`package` must have resources."
  )

  # Resources without name are tested in test-check_package.R
  # Resources without path or data are tested in test-read_resource.R
  unlink(temp_dir, recursive = TRUE)
})

test_that("write_package() writes to the specified directory", {
  pkg <- suppressMessages(read_package(
    system.file("extdata", "datapackage.json", package = "frictionless")
  ))
  temp_subdir <- file.path(tempdir(), "x/y")

  # Function should create subdir(s) without error
  expect_invisible(write_package(pkg, temp_subdir))

  # Valid package can be found at temp_subdir
  expect_true(check_package(
    suppressMessages(read_package(file.path(temp_subdir, "datapackage.json")))
  ))
  unlink(temp_subdir, recursive = TRUE)
})

test_that("write_package() writes unaltered datapackage.json as is", {
  pkg <- suppressMessages(read_package(
    system.file("extdata", "datapackage.json", package = "frictionless")
  ))
  json_in <- readr::read_file(
    system.file("extdata", "datapackage.json", package = "frictionless")
  )
  temp_dir <- tempdir()
  write_package(pkg, temp_dir)
  json_out <- readr::read_file(file.path(temp_dir, "datapackage.json"))

  # Output json = input json. This also tests if new properties (resource_names,
  # directories) are removed and json is printed "pretty"
  expect_equal(json_out, json_in)
  unlink(temp_dir, recursive = TRUE)
})

test_that("write_package() leaves Data Resources with URL as is (no copying)", {

})

test_that("write_package() copies files for Data Resources with path, but does not read them", {

})

test_that("write_packages() creates files for new data.frame Data Resources", {

})

test_that("write_package() adds Data Resource properties based on write_csv() behaviour", {

})
