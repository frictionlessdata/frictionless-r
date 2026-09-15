# Return ----
test_that("add_properties() returns a list with added properties", {
  list <- list(a = 1, b = 2)
  expect_identical(
    add_properties(list, c = 3, d = list(e = 5)),
    list(a = 1, b = 2, c = 3, d = list(e = 5))
  )
  vector <- c(1, 2, 3)
  expect_identical(
    add_properties(vector, d = 4),
    list(c(1), c(2), c(3), d = 4)
  )
})

# Error handling ----
test_that("add_properties() returns error if ... arguments are unnamed", {
  list <- list(a = 1, b = 2)
  expect_error(
    add_properties(list, 3),
    class = "frictionless_error_dots_argument_unnamed"
  )
  expect_error(
    add_properties(list, c = 3, 4),
    class = "frictionless_error_dots_argument_unnamed"
  )
  expect_error(
    add_properties(list, c = 3, 4, after = 0),
    class = "frictionless_error_dots_argument_unnamed"
  )
})

# Functionality ----
test_that("add_properties() can insert properties using after", {
  list <- list(a = 1, b = 2)
  expect_identical(
    add_properties(list, c = 3, d = 4, after = 0),
    list(c = 3, d = 4, a = 1, b = 2)
  )
  expect_identical(
    add_properties(list, c = 3, d = 4, after = length(list) - 1),
    list(a = 1, c = 3, d = 4, b = 2)
  )
})

test_that("add_properties() preserves custom attributes and classes", {
  list <- list(a = 1, b = 2)
  attr(list, "custom_attribute") <- "custom_value" # Add custom attribute
  class(list) <- c("custom_class", class(list)) # Add custom class
  expect_identical(
    attr(add_properties(list, c = 3), "custom_attribute"),
    "custom_value"
  )
  expect_s3_class(add_properties(list, c = 3), c("custom_class", "list"))
})

test_that("add_properties() preserves names", {
  vector <- head(letters, -1L) # a, b, ... y
  vector <- purrr::set_names(vector, toupper(vector)) # Add names
  expect_named(add_properties(vector, Z = "z"), toupper(letters))
})

test_that("add_properties() can return a valid package", {
  p <- create_package()
  p_appended <- add_properties(p, custom_property = "custom_value", after = 0)
  expect_no_error(check_package(p_appended))
  expect_identical(names(p_appended)[[1]], "custom_property")
})

test_that("add_properties() can return a valid resource", {
  resource <- resource(example_package(), "deployments")
  resource_appended <- add_properties(
    resource,
    custom_property = "custom_value",
    after = 0
  )
  expect_identical(names(resource_appended)[[1]], "custom_property")

  # Resource attributes are kept
  expect_identical(
    attr(resource_appended, "data_location"),
    attr(resource, "data_location")
  )
})

test_that("add_properties() can return a valid schema", {
  schema <- schema(example_package(), "deployments")
  schema_appended <- add_properties(
    schema,
    custom_property = "custom_value",
    after = 0
  )
  expect_identical(names(schema_appended)[[1]], "custom_property")
})
