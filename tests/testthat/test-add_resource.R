test_that("add_resource() returns a valid Data Package", {
  pkg <- example_package
  df <- data.frame(
    "col_1" = c(1, 2),
    "col_2" = factor(c("a", "b"), levels = c("a", "b", "c"))
  )
  schema <- create_schema(df)
  expect_true(check_package(add_resource(pkg, "new", df)))
  expect_true(check_package(add_resource(pkg, "new", df, schema)))
})

test_that("add_resource() returns error on incorrect Data Package", {
  df <- data.frame(
    "col_1" = c(1, 2),
    "col_2" = factor(c("a", "b"), levels = c("a", "b", "c"))
  )
  expect_error(
    add_resource(list(), "new", df),
    "`package` must be a list object of class `datapackage`"
  )
})

test_that("add_resource() returns error when resource name contains invalid characters", {
  pkg <- example_package
  df <- data.frame(
    "col_1" = c(1, 2),
    "col_2" = factor(c("a", "b"), levels = c("a", "b", "c"))
  )
  expect_error(add_resource(pkg, "New", df), "only contain lowercase")
  expect_error(add_resource(pkg, "nëw", df), "only contain lowercase")
  expect_error(add_resource(pkg, " new", df), "only contain lowercase")
  expect_error(add_resource(pkg, "new ", df), "only contain lowercase")
  expect_error(add_resource(pkg, "n ew", df), "only contain lowercase")
  expect_error(add_resource(pkg, "n/ew", df), "only contain lowercase")

  expect_true(check_package(add_resource(pkg, "n.ew", df)))
  expect_true(check_package(add_resource(pkg, "n-ew", df)))
  expect_true(check_package(add_resource(pkg, "n_ew", df)))
  expect_true(check_package(add_resource(pkg, "n3w", df)))
  expect_true(check_package(add_resource(pkg, "n.3-w_10", df)))
})

test_that("add_resource() returns error when resource of that name already exists", {
  pkg <- example_package
  df <- data.frame(
    "col_1" = c(1, 2),
    "col_2" = factor(c("a", "b"), levels = c("a", "b", "c"))
  )
  expect_error(
    add_resource(pkg, "deployments", df),
    "`package` already contains a resource named `deployments`."
  )
})

test_that("add_resource() returns error on invalid or empty data frame", {
  pkg <- example_package
  df <- data.frame(
    "col_1" = c(1, 2),
    "col_2" = factor(c("a", "b"), levels = c("a", "b", "c"))
  )
  schema <- create_schema(df)
  expect_error(
    add_resource(pkg, "new", data.frame("col_1" = character(0))),
    "`df` must be a data frame containing data."
  )
  expect_error(
    add_resource(pkg, "new", data.frame("col_1" = character(0)), schema),
    "`df` must be a data frame containing data."
  )

  # For more tests see test-check_schema.R
})

test_that("add_resource() returns error on incorrect Table Schema", {
  pkg <- example_package
  df <- data.frame(
    "col_1" = c(1, 2),
    "col_2" = factor(c("a", "b"), levels = c("a", "b", "c"))
  )
  schema_invalid <- create_schema(df) # Not yet invalid
  schema_invalid[[1]]$name <- "no_such_col"
  expect_error(add_resource(pkg, "new", df, schema_invalid))

  # For more tests see test-check_schema.R
})

test_that("add_resource() adds resource, resource_name", {
  pkg <- example_package
  df <- data.frame(
    "col_1" = c(1, 2),
    "col_2" = factor(c("a", "b"), levels = c("a", "b", "c"))
  )
  pkg <- add_resource(pkg, "new", df)

  # Resource added
  expect_length(pkg$resources, 4) # Remains a list, now of length 4
  expect_equal(pkg$resources[[4]][["name"]], "new")

  # Resource name added
  expect_equal(
    pkg$resource_names,
    c("deployments", "observations", "media", "new")
  )
})

test_that("add_resource() adds schema when none is provided", {
  pkg <- example_package
  df <- data.frame(
    "col_1" = c(1, 2),
    "col_2" = factor(c("a", "b"), levels = c("a", "b", "c"))
  )
  pkg <- add_resource(pkg, "new", df)
  expect_equal(pkg$resources[[4]]$schema, create_schema(df))
})

test_that("add_resource() creates resource that can be passed to read_resource()", {
  pkg <- example_package
  df <- data.frame(
    "col_1" = c(1, 2),
    "col_2" = factor(c("a", "b"), levels = c("a", "b", "c"))
  )
  pkg <- add_resource(pkg, "new", df)
  expect_equal(read_resource(pkg, "new"), dplyr::as_tibble(df))
})

test_that("add_resource() creates resource that can be passed to get_schema()", {
  pkg <- example_package
  df <- data.frame(
    "col_1" = c(1, 2),
    "col_2" = factor(c("a", "b"), levels = c("a", "b", "c"))
  )
  schema <- create_schema(df)
  pkg <- add_resource(pkg, "new", df, schema)
  expect_equal(get_schema(pkg, "new"), schema)
})

if (FALSE) {
  test_that("add_resource() creates resource that can be passed to write_package()", {
    pkg <- example_package
    df <- data.frame(
      "col_1" = c(1, 2),
      "col_2" = factor(c("a", "b"), levels = c("a", "b", "c"))
    )
    pkg <- add_resource(pkg, "new", df)
    temp_dir <- tempdir()
    expect_invisible(write_package(pkg, temp_dir)) # Can write successfully
    unlink(temp_dir, recursive = TRUE)
  })
}
