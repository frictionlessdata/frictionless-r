test_that("create_schema() returns error on incorrect df", {
  expect_error(create_schema("not_a_df"), "`df` must be a data frame.")
})

test_that("create_schema() accepts data frames and tibbles", {
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  tibble <- tibble("col_1" = c(1, 2), "col_2" = c("a", "b"))
  expect_type(create_schema(df), "list")
  expect_type(create_schema(tibble), "list")
})

test_that("create_schema() returns a Table Schema as a list without empty properties", {
  df <- data.frame(
    "col_1" = c(1, 2),
    "col_2" = factor(c("a", "b"), levels = c("a", "b", "c"))
  )
  expected_schema <- list(
    fields = list(
      list(
        name = "col_1",
        type = "number"
        # No constraints
      ),
      list(
        name = "col_2",
        type = "string",
        constraints = list(
          enum = c("a", "b", "c")
        )
      )
    )
  )
  expect_equal(create_schema(df), expected_schema)
})

test_that("create_schema() uses colnames as field names", {
  df <- data.frame(NA, NA, NA, NA)
  colnames <- c("col_1", "Column 2", "col_3!") # Only 3 of 4 defined
  colnames(df) <- colnames
  expect_equal(
    map_chr(create_schema(df)$fields, ~ .x$name), # Vector with field$name
    c(colnames, "") # Last unnamed column (NA) should have name ""
  )
})

test_that("create_schema() translates coltypes into field types", {

})
