test_that("as.list() converts a package to a list", {
  p <- example_package
  expect_s3_class(as.list(p), NA) # S3 class removed
})

test_that("as.list() can be reverted with create_package()", {
  p <- example_package
  expect_equal(
    create_package(as.list(p)),
    p
  )
})
