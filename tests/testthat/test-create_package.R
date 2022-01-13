test_that("create_package() returns a valid Data Package", {
  expect_true(check_package(create_package()))
})

test_that("create_package() sets profile to 'tabular-data-package'", {
  p <- create_package()
  expect_identical(p$profile, "tabular-data-package")
})
