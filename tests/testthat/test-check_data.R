test_that("check_data() returns data invisibly on valid, non-empty data frame", {
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  expect_identical(check_data(df), df)
  expect_invisible(check_data(df))
})

test_that("check_data() returns error on invalid or empty data frame", {
  expect_error(
    check_data("not_a_df"),
    class = "frictionless_error_data_invalid"
  )
  expect_error(
    check_data(data.frame()),
    class = "frictionless_error_data_invalid"
  )
  expect_error(
    check_data(data.frame("col_1" = character(0))),
    class = "frictionless_error_data_invalid"
  )
  expect_error(
    create_schema("not_a_df"),
    regexp = "`data` must be a data frame containing data.",
    fixed = TRUE
  )
})
