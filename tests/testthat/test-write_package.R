test_that("write_package() returns error on incorrect package", {
  expect_error(
    write_package(list()),
    "`package` must be a list object of class `datapackage`"
  )
})

test_that("write_package() returns error if package has no resource(s)", {
  pkg_empty <- create_package()
  expect_error(
    write_package(pkg_empty, file.path(tempdir())),
    "`package` must have resources."
  )

  # Resources without name are tested in test-check_package.R
  # Resources without path or data are tested in test-read_resource.R
})

test_that("write_package() writes to the specified directory", {
  pkg <- suppressMessages(read_package(
    system.file("extdata", "datapackage.json", package = "frictionless")
  ))
  temp_dir <- file.path(tempdir(), "subdir")

  # Write and read to non-existing subdir are successful
  expect_invisible(write_package(pkg, temp_dir))
  expect_true(
    suppressMessages(read_package(file.path(temp_dir, "datapackage.json"))) %>%
    check_package()
  )
})

test_that("write_package() writes a valid, pretty datapackage.json", {

})

test_that("write_package() removes properties resource_names, directory", {

})

test_that("write_package() leaves resources with URL as is", {

})

test_that("write_package() copies files for resources with path, but does not read them", {

})

test_that("write_packages() creates files for new data.frame resources", {

})

test_that("write_package() adds Data Resource properties based on write_csv() behaviour", {

})
