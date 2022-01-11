test_that("add_resource() returns a valid Data Package", {
  pkg <- example_package
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  df_csv <- "data/df.csv"
  schema <- create_schema(df)
  expect_true(check_package(add_resource(pkg, "new", df)))
  expect_true(check_package(add_resource(pkg, "new", df, schema)))
  expect_true(check_package(add_resource(pkg, "new", df_csv)))
})

test_that("add_resource() returns error on incorrect Data Package", {
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  expect_error(
    add_resource(list(), "new", df),
    "`package` must be a list object of class `datapackage`",
    fixed = TRUE
  )
})

test_that("add_resource() returns error when resource name contains invalid
           characters", {
  pkg <- example_package
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  expect_error(
    add_resource(pkg, "New", df),
    paste(
      "`New` must only contain lowercase alphanumeric characters plus",
      "`.`, `-` and `_`."
    ),
    fixed = TRUE
  )
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

test_that("add_resource() returns error when resource of that name already
           exists", {
  pkg <- example_package
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  expect_error(
    add_resource(pkg, "deployments", df),
    "`package` already contains a resource named `deployments`.",
    fixed = TRUE
  )
})

test_that("add_resource() returns error when data is not data frame or
           character", {
  pkg <- example_package
  expect_error(
    add_resource(pkg, "new", list()),
    "`data` must be a data frame or path(s) to CSV file(s).",
    fixed = TRUE
  )
})

test_that("add_resource() returns error on invalid or empty data frame", {
  pkg <- example_package
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  schema <- create_schema(df)
  expect_error(
    add_resource(pkg, "new", data.frame("col_1" = character(0))),
    "`data` must be a data frame containing data.",
    fixed = TRUE
  )
  expect_error(
    add_resource(pkg, "new", data.frame("col_1" = character(0)), schema),
    "`data` must be a data frame containing data.",
    fixed = TRUE
  )

  # For more tests see test-check_schema.R
})

test_that("add_resource() returns error if CSV file cannot be found", {
  pkg <- example_package
  df_csv <- "data/df.csv"
  schema <- create_schema(data.frame("col_1" = c(1, 2), "col_2" = c("a", "b")))
  expect_error(
    add_resource(pkg, "new", "no_such_file.csv"),
    "Can't find file at `no_such_file.csv`.",
    fixed = TRUE
  )
  expect_error(
    add_resource(pkg, "new", "no_such_file.csv", schema),
    "Can't find file at `no_such_file.csv`.",
    fixed = TRUE
  )
  expect_error(
    add_resource(pkg, "new", "http://example.com/no_such_file.csv"),
    "Can't find file at `http://example.com/no_such_file.csv`.",
    fixed = TRUE
  )
  expect_error(
    add_resource(pkg, "new", c(df_csv, "no_such_file.csv")),
    "Can't find file at `no_such_file.csv`.",
    fixed = TRUE
  )
  expect_error(
    add_resource(pkg, "new", c("no_such_file.csv", df_csv)),
    "Can't find file at `no_such_file.csv`.",
    fixed = TRUE
  )
  expect_error(
    add_resource(pkg, "new", c("no_such_file_1.csv", "no_such_file_2.csv")),
    "Can't find file at `no_such_file_1.csv`.",
    fixed = TRUE
  )
})

test_that("add_resource() returns error on mismatching schema and data", {
  pkg <- example_package
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  df_csv <- "data/df.csv"
  schema_invalid <- create_schema(df) # Not yet invalid
  schema_invalid$fields[[1]]$name <- "no_such_col"

  # df
  expect_error(
    add_resource(pkg, "new", df, schema_invalid),
    paste(
      "Field names in `schema` must match column names in data:",
      "ℹ Field names: `no_such_col`, `col_2`",
      "ℹ Column names: `col_1`, `col_2`",
      sep = "\n"
    ),
    fixed = TRUE
  )

  # csv
  expect_error(
    add_resource(pkg, "new", df_csv, schema_invalid),
    paste(
      "Field names in `schema` must match column names in data:",
      "ℹ Field names: `no_such_col`, `col_2`",
      "ℹ Column names: `col_1`, `col_2`",
      sep = "\n"
    ),
    fixed = TRUE
  )

  # For more tests see test-check_schema.R
})

test_that("add_resource() adds resource, reource_name", {
  pkg <- example_package
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  df_csv <- "data/df.csv"

  # df
  pkg <- add_resource(pkg, "new_df", df)
  expect_length(pkg$resources, 4) # Remains a list, now of length 4
  expect_identical(pkg$resources[[4]][["name"]], "new_df")
  expect_identical(pkg$resources[[4]][["profile"]], "tabular-data-resource")
  expect_identical(pkg$resources[[4]][["data"]], df)
  expect_identical(
    pkg$resource_names,
    c("deployments", "observations", "media", "new_df")
  )

  # csv
  pkg <- add_resource(pkg, "new_csv", df_csv)
  expect_length(pkg$resources, 5) # Remains a list, now of length 5
  expect_identical(pkg$resources[[5]][["name"]], "new_csv")
  expect_identical(pkg$resources[[5]][["profile"]], "tabular-data-resource")
  expect_identical(pkg$resources[[5]][["data"]], NULL)
  expect_identical(
    pkg$resource_names,
    c("deployments", "observations", "media", "new_df", "new_csv")
  )
})

test_that("add_resource() uses provided schema or creates one", {
  pkg <- example_package
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  df_csv <- "data/df.csv"
  schema <- create_schema(df)
  schema_custom <- list(fields = list(
    list(name = "col_1", type = "number", title = "Column 1"),
    list(name = "col_2", type = "string", title = "Column 2")
  ))

  # df
  pkg <- add_resource(pkg, "new_df", df)
  pkg <- add_resource(pkg, "new_df_with_schema", df, schema_custom)
  expect_identical(pkg$resources[[4]]$schema, schema)
  expect_identical(pkg$resources[[5]]$schema, schema_custom)
  expect_identical(get_schema(pkg, "new_df"), schema)
  expect_identical(get_schema(pkg, "new_df_with_schema"), schema_custom)

  # csv
  pkg <- add_resource(pkg, "new_csv", df)
  pkg <- add_resource(pkg, "new_csv_with_schema", df, schema_custom)
  expect_identical(pkg$resources[[6]]$schema, schema)
  expect_identical(pkg$resources[[7]]$schema, schema_custom)
  expect_identical(get_schema(pkg, "new_csv"), schema)
  expect_identical(get_schema(pkg, "new_csv_with_schema"), schema_custom)
})

test_that("add_resource() can add resource from data frame, readable by
           read_resource()", {
  pkg <- example_package
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  pkg <- add_resource(pkg, "new", df)
  expect_identical(read_resource(pkg, "new"), dplyr::as_tibble(df))
})

test_that("add_resource() can add resource from local, remote, relative,
           absolute or compressed CSV file, readable by read_resource()", {
  pkg <- example_package
  schema <- get_schema(pkg, "deployments")

  # Local
  local_path <- "data/df.csv"
  pkg <- add_resource(pkg, "local", local_path)
  expect_identical(pkg$resources[[4]]$path, local_path)
  expect_s3_class(read_resource(pkg, "local"), "tbl")

  # Remote
  remote_path <- file.path(
    "https://github.com/frictionlessdata/frictionless-r",
    "raw/main/inst/extdata/deployments.csv"
  )
  pkg <- add_resource(pkg, "remote", remote_path, schema)
  expect_identical(pkg$resources[[5]]$path, remote_path)
  expect_s3_class(read_resource(pkg, "remote"), "tbl")

  # Relative (doesn't throw unsafe error)
  relative_path <- "../../inst/extdata/deployments.csv"
  pkg <- add_resource(pkg, "relative", relative_path, schema)
  expect_identical(pkg$resources[[6]]$path, relative_path)
  expect_s3_class(read_resource(pkg, "relative"), "tbl")

  # Absolute (doesn't throw unsafe error)
  absolute_path <- system.file(
    "extdata", "deployments.csv", package = "frictionless" # Will start with /
  )
  pkg <- add_resource(pkg, "absolute", absolute_path, schema)
  expect_identical(pkg$resources[[7]]$path, absolute_path)
  expect_s3_class(read_resource(pkg, "absolute"), "tbl")

  # Compressed
  compressed_file <- "data/deployments.csv.gz"
  pkg <- add_resource(pkg, "compressed", compressed_file, schema)
  expect_identical(pkg$resources[[8]]$path, compressed_file)
  expect_s3_class(read_resource(pkg, "compressed"), "tbl")
})

test_that("add_resource() can add resource from CSV file with other delimiter,
           readable by read_resource()", {
  pkg <- example_package
  pkg <- add_resource(pkg, "df", "data/df.csv")
  pkg <- add_resource(pkg, "df_delim", "data/df_delim.csv", delim = ";")
  expect_identical(pkg$resources[[5]]$dialect$delimiter, ";")
  expect_identical(read_resource(pkg, "df_delim"), read_resource(pkg, "df"))
})

if (FALSE) {
  test_that("add_resource() creates resource that can be passed to
             write_package()", {
    pkg <- example_package
    df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
    pkg <- add_resource(pkg, "new", df)
    temp_dir <- tempdir()
    on.exit(unlink(temp_dir, recursive = TRUE))
    expect_invisible(write_package(pkg, temp_dir)) # Can write successfully
  })
}
