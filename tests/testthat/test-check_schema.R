test_that("check_schema() returns TRUE on valid Table Schema", {
  pkg <- example_package
  df <- read_resource(pkg, "deployments")
  schema <- get_schema(pkg, "deployments")
  expect_true(check_schema(df, schema))

  df_created <- data.frame(
    "col_1" = c(1, 2),
    "col_2" = factor(c("a", "b"), levels = c("a", "b", "c"))
  )
  schema_created <- create_schema(df)
  expect_true(check_schema(df_created, schema_created))
})

test_that("check_schema() returns error on invalid or empty data frame", {
  df <- data.frame(
    "col_1" = c(1, 2),
    "col_2" = factor(c("a", "b"), levels = c("a", "b", "c"))
  )
  schema <- create_schema(df)
  expect_error(
    check_schema("not_a_df", schema),
    "`df` must be a data frame containing data."
  )
  expect_error(
    check_schema(data.frame(), schema),
    "`df` must be a data frame containing data."
  )
})

test_that("check_schema() returns error on incorrect Table Schema", {
  df <- data.frame(
    "col_1" = c(1, 2),
    "col_2" = factor(c("a", "b"), levels = c("a", "b", "c"))
  )
  expect_error(check_schema(df, "not_a_list"))
  expect_error(check_schema(df, list()))
  expect_error(check_schema(df, list(fields = list())))

  # Missing names
  invalid_schema <- list(fields = list(
    list(type = "number"),
    list(type = "string")
  ))
  expect_error(check_schema(df, invalid_schema))

  # Non-matching names
  invalid_schema <- list(fields = list(
    list(name = "col_2", type = "number"),
    list(name = "col_1", type = "string")
  ))
  expect_error(check_schema(df, invalid_schema))

  # Too few elements
  invalid_schema <- list(fields = list(
    list(name = "col_1", type = "number")
  ))
  expect_error(check_schema(df, invalid_schema))

  # Too many elements
  invalid_schema <- list(fields = list(
    list(name = "col_1", type = "number"),
    list(name = "col_2", type = "string"),
    list(name = "col_3", type = "integer")
  ))
  expect_error(check_schema(df, invalid_schema))

  # Types not recognized
  invalid_schema <- list(fields = list(
    list(name = "col_1", type = "number"),
    list(name = "col_2", type = "not_a_type")
  ))
  expect_error(check_schema(df, invalid_schema))
})
