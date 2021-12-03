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
  pkg_remote <- suppressMessages(read_package(
    "https://github.com/frictionlessdata/frictionless-r/raw/main/inst/extdata/datapackage.json"
  ))
  temp_dir <- tempdir()
  write_package(pkg_remote, temp_dir)
  pkg_out <- suppressMessages(read_package(
    file.path(temp_dir, "datapackage.json")
  ))

  # Paths are now URLs
  expect_equal(
    pkg_out$resources[[1]]$path,
    "https://github.com/frictionlessdata/frictionless-r/raw/main/inst/extdata/deployments.csv"
  )
  expect_equal(
    pkg_out$resources[[2]]$path,
    c(
      "https://github.com/frictionlessdata/frictionless-r/raw/main/inst/extdata/observations_1.csv",
      "https://github.com/frictionlessdata/frictionless-r/raw/main/inst/extdata/observations_2.csv"
    )
  )

  # Do not expect files
  expect_error(
    readr::read_file(file.path(temp_dir, "deployments.csv")),
    "does not exist."
  )
  expect_error(
    readr::read_file(file.path(temp_dir, "observations_1.csv")),
    "does not exist."
  )
  expect_error(
    readr::read_file(file.path(temp_dir, "observations_2.csv")),
    "does not exist."
  )
  unlink(temp_dir, recursive = TRUE)
})

test_that("write_package() copies Data Resources with path, but does not read them", {
  pkg <- suppressMessages(read_package(
    system.file("extdata", "datapackage.json", package = "frictionless")
  ))
  temp_dir <- tempdir()
  write_package(pkg, temp_dir)
  pkg_out <- suppressMessages(read_package(
    file.path(temp_dir, "datapackage.json")
  ))

  # Path is unchanged (deployments and observations are local files)
  expect_equal(pkg_out$resources[[1]]$path, pkg$resources[[1]]$path)
  expect_equal(pkg_out$resources[[2]]$path, pkg$resources[[2]]$path)

  # Files are written
  expect_type(readr::read_file(file.path(temp_dir, "deployments.csv")), "character")
  expect_type(readr::read_file(file.path(temp_dir, "observations_1.csv")), "character")
  expect_type(readr::read_file(file.path(temp_dir, "observations_2.csv")), "character")
  unlink(temp_dir, recursive = TRUE)
})

test_that("write_package() leaves existing Data Resources with `data` as is", {
  pkg <- suppressMessages(read_package(
    system.file("extdata", "datapackage.json", package = "frictionless")
  ))
  temp_dir <- tempdir()
  write_package(pkg, temp_dir)
  pkg_out <- suppressMessages(read_package(
    file.path(temp_dir, "datapackage.json")
  ))

  expect_equal(pkg$resources[[3]], pkg_out$resources[[3]])
  unlink(temp_dir, recursive = TRUE)
})

test_that("write_package() creates files for new data.frame Data Resources", {
  pkg <- suppressMessages(read_package(
    system.file("extdata", "datapackage.json", package = "frictionless")
  ))
  df <- data.frame(
    "col_1" = c(1, 2),
    "col_2" = factor(c("a", "b"), levels = c("a", "b", "c"))
  )
  pkg$resources[[3]]$data <- df
  temp_dir <- tempdir()
  write_package(pkg, temp_dir)
  pkg_out <- suppressMessages(read_package(
    file.path(temp_dir, "datapackage.json")
  ))

  # Added resource has path (not data) and was written to file
  expect_equal(pkg_out$resources[[3]]$path, "media.csv")
  expect_null(pkg_out$resources[[3]]$data)
  expect_type(readr::read_file(file.path(temp_dir, "media.csv")), "character")
  unlink(temp_dir, recursive = TRUE)
})

test_that("write_package() adds Data Resource properties based on write_csv() behaviour", {

})
