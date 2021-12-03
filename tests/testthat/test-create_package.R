test_that("create_package() returns a valid Data Package", {
  expect_true(check_package(create_package()))
})
