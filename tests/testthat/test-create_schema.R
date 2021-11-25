test_that("create_schema() returns error on incorrect df", {
  expect_error(create_schema("not_a_df"), "`df` must be a data frame.")
})

test_that("create_schema() accepts data frames and tibbles", {
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  tibble <- tibble("col_1" = c(1, 2), "col_2" = c("a", "b"))
  expect_type(create_schema(df), "list")
  expect_type(create_schema(tibble), "list")
})

test_that("create_schema() returns a Table Schema list with the correct structure", {

})

test_that("create_schema() does not return empty properties", {

})

test_that("create_schema() uses colnames as field names", {
})

test_that("create_schema() translates coltypes into field types", {

})
