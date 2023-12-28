test_that("check_schema() returns TRUE on valid Table Schema", {
  skip_if_offline()
  p <- example_package
  # Can't obtain df using read_resource(), because that function uses
  # check_schema() (in get_schema()) internally, which is what we want to test
  df <- suppressMessages(
    readr::read_csv(file.path(p$directory, p$resources[[1]]$path))
  )
  schema_get <- get_schema(p, "deployments")
  schema_create <- create_schema(df)
  expect_true(check_schema(schema_get))
  expect_true(check_schema(schema_create))
  expect_true(check_schema(schema_get, df))
  expect_true(check_schema(schema_create, df))
})

test_that("check_schema() returns error on invalid Table Schema", {
  # Must be a list and have list property "fields"
  expect_error(
    check_schema("not_a_list"),
    "`schema` must be a list with property `fields`.",
    fixed = TRUE
  )
  expect_error(
    check_schema(list()),
    "`schema` must be a list with property `fields`.",
    fixed = TRUE
  )

  # No names
  invalid_schema <- list(fields = list(
    list(type = "number"),
    list(type = "string")
  ))
  expect_error(
    check_schema(invalid_schema),
    paste(
      "All fields in `schema` must have property `name`.",
      "ℹ Field(s) `1`, `2` don't have a name.",
      sep = "\n"
    ),
    fixed = TRUE
  )

  # Invalid types
  invalid_schema <- list(fields = list(
    list(name = "col_1", type = "number"),
    list(name = "col_2", type = "not_a_type")
  ))
  expect_error(
    check_schema(invalid_schema),
    paste(
      "All fields in `schema` must have valid `type`.",
      "Type `not_a_type` is invalid."
    ),
    fixed = TRUE
  )
  invalid_schema <- list(fields = list(
    list(name = "col_1", type = "not_a_type"),
    list(name = "col_2", type = "not_a_type_either")
  ))
  expect_error(
    check_schema(invalid_schema),
    paste(
      "All fields in `schema` must have valid `type`.",
      "Type `not_a_type`, `not_a_type_either` is invalid."
    ),
    fixed = TRUE
  )
})

test_that("check_schema() allows Table Schema fields to not (all) have type", {
  schema <- list(fields = list(
    list(name = "col_1"),
    list(name = "col_2")
  ))
  expect_true(check_schema(schema))
  schema <- list(fields = list(
    list(name = "col_1", type = "string"),
    list(name = "col_2")
  ))
  expect_true(check_schema(schema))
})

test_that("check_schema() returns error on incorrect or empty data frame", {
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  schema <- create_schema(df)
  expect_error(
    check_schema(schema, "not_a_df"),
    class = "frictionless_error_data_incorrect"
  )
  expect_error(
    check_schema(schema, data.frame()),
    class = "frictionless_error_data_incorrect"
  )
})

test_that("check_schema() returns error on mismatching schema and data frame", {
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))

  # Non-matching names
  invalid_schema <- list(fields = list(
    list(name = "col_2", type = "number"),
    list(name = "col_1", type = "string")
  ))
  expect_error(
    check_schema(invalid_schema, df),
    paste(
      "Field names in `schema` must match column names in data:",
      "ℹ Field names: `col_2`, `col_1`",
      "ℹ Column names: `col_1`, `col_2`",
      sep = "\n"
    ),
    fixed = TRUE
  )

  # Too few elements
  invalid_schema <- list(fields = list(
    list(name = "col_1", type = "number")
  ))
  expect_error(
    check_schema(invalid_schema, df),
    paste(
      "Field names in `schema` must match column names in data:",
      "ℹ Field names: `col_1`",
      "ℹ Column names: `col_1`, `col_2`",
      sep = "\n"
    ),
    fixed = TRUE
  )

  # Too many elements
  invalid_schema <- list(fields = list(
    list(name = "col_1", type = "number"),
    list(name = "col_2", type = "string"),
    list(name = "col_3", type = "integer")
  ))
  expect_error(
    check_schema(invalid_schema, df),
    paste(
      "Field names in `schema` must match column names in data:",
      "ℹ Field names: `col_1`, `col_2`, `col_3`",
      "ℹ Column names: `col_1`, `col_2`",
      sep = "\n"
    ),
    fixed = TRUE
  )
})
