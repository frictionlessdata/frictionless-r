test_that("create_package() returns a valid Data Package", {
  pkg <- create_package()
  expect_true(check_package(pkg))
})
