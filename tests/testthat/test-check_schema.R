test_that("check_schema() returns schema invisibly on valid Table Schema", {
  p <- example_package()

  # Can't obtain df using read_resource(), because that function uses
  # check_schema() (in schema()) internally, which is what we want to test
  df <- suppressMessages(
    readr::read_csv(file.path(p$directory, p$resources[[1]]$path))
  )

  # Using schema()
  schema_get <- schema(p, "deployments")
  expect_identical(check_schema(schema_get), schema_get)
  expect_invisible(check_schema(schema_get))
  expect_identical(check_schema(schema_get, df), schema_get)
  expect_invisible(check_schema(schema_get, df))

  # Using create_schema()
  schema_create <- create_schema(df)
  expect_identical(check_schema(schema_create), schema_create)
  expect_invisible(check_schema(schema_create))
  expect_identical(check_schema(schema_create, df), schema_create)
  expect_invisible(check_schema(schema_create, df))
})

test_that("check_schema() returns error on invalid or empty Table Schema", {
  # Must be a list and have list property "fields"
  expect_error(
    check_schema("not_a_list"),
    class = "frictionless_error_schema_invalid"
  )
  expect_error(
    check_schema(list()),
    class = "frictionless_error_schema_invalid"
  )
  expect_error(
    check_schema("not_a_list"),
    regexp = "`schema` must be a list with a fields property.",
    fixed = TRUE
  )
})

test_that("check_schema() returns error when Table Schema fields don't have
           names", {
  # One missing name
  invalid_schema <- list(fields = list(
    list(name = "col_1", type = "number"),
    list(type = "string")
  ))
  expect_error(
    check_schema(invalid_schema),
    class = "frictionless_error_fields_without_name"
  )
  expect_error(
    check_schema(invalid_schema),
    regexp = "All fields in `schema` must have a name property.",
    fixed = TRUE
  )
  expect_error(
    check_schema(invalid_schema),
    regexp = "Field 2 doesn't have a name.",
    fixed = TRUE
  )

  # All missing names
  invalid_schema <- list(fields = list(
    list(type = "number"),
    list(type = "string")
  ))
  expect_error(
    check_schema(invalid_schema),
    class = "frictionless_error_fields_without_name"
  )
  expect_error(
    check_schema(invalid_schema),
    regexp = "Fields 1 and 2 don't have a name.",
    fixed = TRUE
  )
})

test_that("check_schema() returns error when Table Schema fields have invalid
           types", {
  # One invalid types
  invalid_schema <- list(fields = list(
    list(name = "col_1", type = "number"),
    list(name = "col_2", type = "not_a_type")
  ))
  expect_error(
    check_schema(invalid_schema),
    class = "frictionless_error_fields_type_invalid"
  )
  expect_error(
    check_schema(invalid_schema),
    regexp = "All fields in `schema` must have a valid type property.",
    fixed = TRUE
  )
  expect_error(
    check_schema(invalid_schema),
    regexp = "Type \"not_a_type\" is invalid.",
    fixed = TRUE
  )
  # All invalid types
  invalid_schema <- list(fields = list(
    list(name = "col_1", type = "not_a_type"),
    list(name = "col_2", type = "not_a_type_either")
  ))
  expect_error(
    check_schema(invalid_schema),
    class = "frictionless_error_fields_type_invalid"
  )
  expect_error(
    check_schema(invalid_schema),
    regexp = "Types \"not_a_type\" and \"not_a_type_either\" are invalid.",
    fixed = TRUE
  )
})

test_that("check_schema() allows Table Schema fields to not (all) have type", {
  schema <- list(fields = list(
    list(name = "col_1"),
    list(name = "col_2")
  ))
  expect_no_error(check_schema(schema))
  schema <- list(fields = list(
    list(name = "col_1", type = "string"),
    list(name = "col_2")
  ))
  expect_no_error(check_schema(schema))
})

test_that("check_schema() returns error on invalid or empty data frame", {
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  schema <- create_schema(df)
  expect_error(
    check_schema(schema, "not_a_df"),
    class = "frictionless_error_data_invalid"
  )
  expect_error(
    check_schema(schema, data.frame()),
    class = "frictionless_error_data_invalid"
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
    class = "frictionless_error_fields_colnames_mismatch"
  )
  expect_error(
    check_schema(invalid_schema, df),
    regexp = "Field names in `schema` must match column names in `data`.",
    fixed = TRUE
  )
  expect_error(
    check_schema(invalid_schema, df),
    regexp = "Field names: \"col_2\" and \"col_1\".",
    fixed = TRUE
  )
  expect_error(
    check_schema(invalid_schema, df),
    regexp = "Column names: \"col_1\" and \"col_2\".", # Same for other tests
    fixed = TRUE
  )

  # Too few elements
  invalid_schema <- list(fields = list(
    list(name = "col_1", type = "number")
  ))
  expect_error(
    check_schema(invalid_schema, df),
    class = "frictionless_error_fields_colnames_mismatch"
  )
  expect_error(
    check_schema(invalid_schema, df),
    regexp = "Field name: \"col_1\"",
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
    class = "frictionless_error_fields_colnames_mismatch"
  )
  expect_error(
    check_schema(invalid_schema, df),
    regexp = "Field names: \"col_1\", \"col_2\", and \"col_3\".",
    fixed = TRUE
  )
})
