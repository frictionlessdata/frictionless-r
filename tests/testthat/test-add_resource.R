test_that("add_resource() returns a valid Data Package", {
  pkg <- example_package
  df <- data.frame(
    "col_1" = c(1, 2),
    "col_2" = factor(c("a", "b"), levels = c("a", "b", "c"))
  )
  schema <- create_schema(df)
  expect_true(check_package(add_resource(pkg, "positions", df)))
  expect_true(check_package(add_resource(pkg, "positions", df, schema)))
})

test_that("add_resource() returns error on incorrect Data Package", {
  df <- data.frame(
    "col_1" = c(1, 2),
    "col_2" = factor(c("a", "b"), levels = c("a", "b", "c"))
  )
  expect_error(
    add_resource(list(), "positions", df),
    "`package` must be a list object of class `datapackage`"
  )
})

test_that("add_resource() returns error when resource name contains invalid characters", {
  pkg <- example_package
  df <- data.frame(
    "col_1" = c(1, 2),
    "col_2" = factor(c("a", "b"), levels = c("a", "b", "c"))
  )
  expect_error(add_resource(pkg, "Positions", df), "only contain lowercase")
  expect_error(add_resource(pkg, "pøsitions", df), "only contain lowercase")
  expect_error(add_resource(pkg, " positions", df), "only contain lowercase")
  expect_error(add_resource(pkg, "positions ", df), "only contain lowercase")
  expect_error(add_resource(pkg, "posi tions", df), "only contain lowercase")
  expect_error(add_resource(pkg, "posi/tions", df), "only contain lowercase")

  expect_true(check_package(add_resource(pkg, "posi.tions", df)))
  expect_true(check_package(add_resource(pkg, "posi-tions", df)))
  expect_true(check_package(add_resource(pkg, "posi_tions", df)))
  expect_true(check_package(add_resource(pkg, "p0sitions", df)))
  expect_true(check_package(add_resource(pkg, "p0.si-t1_ons10", df)))
})

test_that("add_resource() returns error when Data Resource of that name already exists", {
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
    add_resource(pkg, "positions", "not_a_df"),
    "`df` must be a data frame with columns."
  )
  expect_error(
    add_resource(pkg, "positions", "not_a_df", schema),
    "`df` must be a data frame with columns."
  )
  expect_error(
    add_resource(pkg, "positions", data.frame()),
    "`df` must be a data frame with columns."
  )
  expect_error(
    add_resource(pkg, "positions", data.frame(), schema),
    "`df` must be a data frame with columns."
  )
})

test_that("add_resource() returns error on incorrect Table Schema", {
  pkg <- example_package
  df <- data.frame(
    "col_1" = c(1, 2),
    "col_2" = factor(c("a", "b"), levels = c("a", "b", "c"))
  )
  schema_invalid <- create_schema(df)
  schema_invalid[[1]]$name <- "no_such_col"
  expect_error(add_resource(pkg, "positions", df, schema_invalid))

  # For more tests see test-check_schema.R
})

test_that("add_resource() adds resource, resource_name", {
  pkg <- example_package
  df <- data.frame(
    "col_1" = c(1, 2),
    "col_2" = factor(c("a", "b"), levels = c("a", "b", "c"))
  )
  pkg_added <- add_resource(pkg, "positions", df)

  # Resource added
  expect_length(pkg_added$resources, 4) # Remains a list, now of length 4
  expect_equal(pkg_added$resources[[4]][["name"]], "positions")

  # Resource name added
  expect_equal(
    pkg_added$resource_names,
    c("deployments", "observations", "media", "positions")
  )
})

test_that("add_resource() adds schema when none is provided", {
  pkg <- example_package
  df <- data.frame(
    "col_1" = c(1, 2),
    "col_2" = factor(c("a", "b"), levels = c("a", "b", "c"))
  )
  pkg_added <- add_resource(pkg, "positions", df)
  expect_equal(pkg_added$resources[[4]]$schema, create_schema(df))
})

test_that("add_resource() creates resource that can be passed to read_resource()", {
  pkg <- example_package
  df <- data.frame(
    "col_1" = c(1, 2),
    "col_2" = factor(c("a", "b"), levels = c("a", "b", "c"))
  )
  pkg_added <- add_resource(pkg, "positions", df)
  expect_equal(read_resource(pkg_added, "positions"), dplyr::as_tibble(df))
})

test_that("add_resource() creates resource that can be passed to get_schema()", {
  pkg <- example_package
  df <- data.frame(
    "col_1" = c(1, 2),
    "col_2" = factor(c("a", "b"), levels = c("a", "b", "c"))
  )
  schema <- create_schema(df)
  pkg_added <- add_resource(pkg, "positions", df, schema)
  expect_equal(get_schema(pkg_added, "positions"), schema)
})

test_that("add_resource() creates resource that can be passed to write_package()", {
  pkg <- example_package
  df <- data.frame(
    "col_1" = c(1, 2),
    "col_2" = factor(c("a", "b"), levels = c("a", "b", "c"))
  )
  pkg_added <- add_resource(pkg, "positions", df)
  temp_dir <- tempdir()
  expect_invisible(write_package(pkg_added, temp_dir)) # Can write successfully
  unlink(temp_dir, recursive = TRUE)
})