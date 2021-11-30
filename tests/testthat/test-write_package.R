test_that("write_package() returns error on incorrect package", {
  expect_error(
    write_package(list()),
    "`package` must be a list object of class `datapackage`"
  )
})

test_that("write_package() returns error if no resources are associated", {

})

test_that("write_package() writes to the specified directory", {

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
