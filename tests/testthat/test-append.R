test_that("datapackages can be appended without losing their class", {
  # Create a datapackage to append
  datapackage_to_append <- create_package()
  # Append with a value
  datapackage_appended <- append(datapackage_to_append, c(ten = "forward"))
  # Check for class
  expect_s3_class(
    append(datapackage_appended, c(ten = "forward")),
    "datapackage"
  )
})
