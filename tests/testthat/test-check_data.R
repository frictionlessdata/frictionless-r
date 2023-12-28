test_that("check_data() returns error on incorrect or empty data frame", {
  expect_error(
    check_data("not_a_df"),
    class = "frictionless_error_data_incorrect"
  )
  expect_error(
    check_data(data.frame()),
    class = "frictionless_error_data_incorrect"
  )
  expect_error(
    check_data(data.frame("col_1" = character(0))),
    class = "frictionless_error_data_incorrect"
  )
  expect_error(
    create_schema(0),
    regexp = "`data` must be a data frame containing data.",
    fixed = TRUE
  )
  expect_error(
    create_schema(0),
    regexp = "`data` is a number.",
    fixed = TRUE
  )
})
