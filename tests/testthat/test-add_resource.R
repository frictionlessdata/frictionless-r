test_that("add_resource() returns a valid Data Package", {
  p <- example_package
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  df_csv <- test_path("data/df.csv")
  schema <- create_schema(df)
  expect_true(check_package(add_resource(p, "new", df)))
  expect_true(check_package(add_resource(p, "new", df, schema)))
  expect_true(check_package(add_resource(p, "new", df_csv)))
})

test_that("add_resource() returns error on incorrect Data Package", {
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  expect_error(
    add_resource(list(), "new", df),
    paste(
      "`package` must be a list describing a Data Package,",
      "created with `read_package()` or `create_package()`."
    ),
    fixed = TRUE
  )
})

test_that("add_resource() returns error when resource name contains invalid
           characters", {
  p <- example_package
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  expect_error(
    add_resource(p, "New", df),
    paste(
      "`New` must only contain lowercase alphanumeric characters plus",
      "`.`, `-` and `_`."
    ),
    fixed = TRUE
  )
  expect_error(add_resource(p, "nëw", df), "only contain lowercase")
  expect_error(add_resource(p, " new", df), "only contain lowercase")
  expect_error(add_resource(p, "new ", df), "only contain lowercase")
  expect_error(add_resource(p, "n ew", df), "only contain lowercase")
  expect_error(add_resource(p, "n/ew", df), "only contain lowercase")

  expect_true(check_package(add_resource(p, "n.ew", df)))
  expect_true(check_package(add_resource(p, "n-ew", df)))
  expect_true(check_package(add_resource(p, "n_ew", df)))
  expect_true(check_package(add_resource(p, "n3w", df)))
  expect_true(check_package(add_resource(p, "n.3-w_10", df)))
})

test_that("add_resource() returns error when resource of that name already
           exists", {
  p <- example_package
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  expect_error(
    add_resource(p, "deployments", df),
    "`package` already contains a resource named `deployments`.",
    fixed = TRUE
  )
})

test_that("add_resource() returns error when data is not data frame or
           character", {
  p <- example_package
  expect_error(
    add_resource(p, "new", list()),
    "`data` must be a data frame or path(s) to CSV file(s).",
    fixed = TRUE
  )
})

test_that("add_resource() returns error on invalid or empty data frame", {
  p <- example_package
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  schema <- create_schema(df)
  expect_error(
    add_resource(p, "new", data.frame("col_1" = character(0))),
    "`data` must be a data frame containing data.",
    fixed = TRUE
  )
  expect_error(
    add_resource(p, "new", data.frame("col_1" = character(0)), schema),
    "`data` must be a data frame containing data.",
    fixed = TRUE
  )

  # For more tests see test-check_schema.R
})

test_that("add_resource() returns error if CSV file cannot be found", {
  testthat::skip_if_offline()
  p <- example_package
  df_csv <- test_path("data/df.csv")
  schema <- create_schema(data.frame("col_1" = c(1, 2), "col_2" = c("a", "b")))
  expect_error(
    add_resource(p, "new", "no_such_file.csv"),
    "Can't find file at `no_such_file.csv`.",
    fixed = TRUE
  )
  expect_error(
    add_resource(p, "new", "no_such_file.csv", schema),
    "Can't find file at `no_such_file.csv`.",
    fixed = TRUE
  )
  expect_error(
    add_resource(p, "new", c(df_csv, "no_such_file.csv")),
    "Can't find file at `no_such_file.csv`.",
    fixed = TRUE
  )
  expect_error(
    add_resource(p, "new", c("no_such_file.csv", df_csv)),
    "Can't find file at `no_such_file.csv`.",
    fixed = TRUE
  )
  expect_error(
    add_resource(p, "new", c("no_such_file_1.csv", "no_such_file_2.csv")),
    "Can't find file at `no_such_file_1.csv`.",
    fixed = TRUE
  )
  testthat::skip_if_offline()
  expect_error(
    add_resource(p, "new", "http://example.com/no_such_file.csv"),
    "Can't find file at `http://example.com/no_such_file.csv`.",
    fixed = TRUE
  )
})

test_that("add_resource() returns error on mismatching schema and data", {
  testthat::skip_if_offline()
  p <- example_package
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  df_csv <- test_path("data/df.csv")
  schema_invalid <- create_schema(df) # Not yet invalid
  schema_invalid$fields[[1]]$name <- "no_such_col"

  # df
  expect_error(
    add_resource(p, "new", df, schema_invalid),
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
    add_resource(p, "new", df_csv, schema_invalid),
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

test_that("add_resource() adds resource", {
  testthat::skip_if_offline()
  p <- example_package
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  df_csv <- test_path("data/df.csv")

  # df
  p <- add_resource(p, "new_df", df)
  expect_length(p$resources, 4) # Remains a list, now of length 4
  expect_identical(p$resources[[4]][["name"]], "new_df")
  expect_identical(p$resources[[4]][["profile"]], "tabular-data-resource")
  expect_identical(p$resources[[4]][["data"]], df)
  expect_identical(
    resources(p),
    c("deployments", "observations", "media", "new_df")
  )

  # csv
  p <- add_resource(p, "new_csv", df_csv)
  expect_length(p$resources, 5) # Remains a list, now of length 5
  expect_identical(p$resources[[5]][["name"]], "new_csv")
  expect_identical(p$resources[[5]][["profile"]], "tabular-data-resource")
  expect_identical(p$resources[[5]][["data"]], NULL)
  expect_identical(
    resources(p),
    c("deployments", "observations", "media", "new_df", "new_csv")
  )
})

test_that("add_resource() uses provided schema (list or path) or creates one", {
  testthat::skip_if_offline()
  p <- create_package()
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  df_csv <- test_path("data/df.csv")
  schema <- create_schema(df)
  schema_custom <- list(fields = list(
    list(name = "col_1", type = "number", title = "Column 1"),
    list(name = "col_2", type = "string", title = "Column 2")
  ))
  schema_file <- test_path("data/schema_custom.json")

  # df
  p <- add_resource(p, "new_df", df)
  p <- add_resource(p, "new_df_with_list_schema", df, schema_custom)
  p <- add_resource(p, "new_df_with_file_schema", df, schema_file)
  expect_identical(p$resources[[1]]$schema, schema)
  expect_identical(p$resources[[2]]$schema, schema_custom)
  expect_identical(p$resources[[3]]$schema, schema_custom)
  expect_identical(get_schema(p, "new_df"), schema)
  expect_identical(get_schema(p, "new_df_with_list_schema"), schema_custom)
  expect_identical(get_schema(p, "new_df_with_file_schema"), schema_custom)

  # csv
  p <- add_resource(p, "new_csv", df)
  p <- add_resource(p, "new_csv_with_list_schema", df, schema_custom)
  p <- add_resource(p, "new_csv_with_file_schema", df, schema_file)
  expect_identical(p$resources[[4]]$schema, schema)
  expect_identical(p$resources[[5]]$schema, schema_custom)
  expect_identical(p$resources[[6]]$schema, schema_custom)
  expect_identical(get_schema(p, "new_csv"), schema)
  expect_identical(get_schema(p, "new_csv_with_list_schema"), schema_custom)
  expect_identical(get_schema(p, "new_csv_with_file_schema"), schema_custom)
})

test_that("add_resource() can add resource from data frame, readable by
           read_resource()", {
  testthat::skip_if_offline()
  p <- example_package
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  p <- add_resource(p, "new", df)
  expect_identical(read_resource(p, "new"), dplyr::as_tibble(df))
})

test_that("add_resource() can add resource from local, relative, absolute,
           remote or compressed CSV file, readable by read_resource()", {
  testthat::skip_if_offline()
  p <- example_package
  schema <- get_schema(p, "deployments")

  # Local
  local_path <- "data/df.csv"
  p <- add_resource(p, "local", local_path)
  expect_identical(p$resources[[4]]$path, local_path)
  expect_s3_class(read_resource(p, "local"), "tbl")

  # Relative (doesn't throw unsafe error)
  relative_path <- "../testthat/data/df.csv"
  p <- add_resource(p, "relative", relative_path)
  expect_identical(p$resources[[5]]$path, relative_path)
  expect_s3_class(read_resource(p, "relative"), "tbl")

  # Absolute (doesn't throw unsafe error)
  absolute_path <- system.file(
    "extdata", "deployments.csv", package = "frictionless" # Will start with /
  )
  p <- add_resource(p, "absolute", absolute_path, schema)
  expect_identical(p$resources[[6]]$path, absolute_path)
  expect_s3_class(read_resource(p, "absolute"), "tbl")

  # Compressed
  compressed_file <- test_path("data/deployments.csv.gz")
  p <- add_resource(p, "compressed", compressed_file, schema)
  expect_identical(p$resources[[8]]$path, compressed_file)
  expect_s3_class(read_resource(p, "compressed"), "tbl")

  # Remote
  remote_path <- file.path(
    "https://github.com/frictionlessdata/frictionless-r",
    "raw/main/inst/extdata/deployments.csv"
  )
  p <- add_resource(p, "remote", remote_path, schema)
  expect_identical(p$resources[[7]]$path, remote_path)
  expect_s3_class(read_resource(p, "remote"), "tbl")
})

test_that("add_resource() can add resource from CSV file with other delimiter,
           readable by read_resource()", {
  p <- create_package()
  p <- add_resource(p, "df", test_path("data/df.csv"))
  expect_identical(p$resources[[1]]$dialect$delimiter, NULL)
  p <- add_resource(p, "df_delim_1", test_path("data/df_delim_1.txt"),
                    delim = ";")
  expect_identical(p$resources[[2]]$dialect$delimiter, ";")
  expect_identical(read_resource(p, "df_delim_1"), read_resource(p, "df"))
  p <- add_resource(p, "df_delim_2", test_path("data/df_delim_2.tsv"),
                    delim = "\t")
  expect_identical(p$resources[[3]]$dialect$delimiter, "\t")
  expect_identical(read_resource(p, "df_delim_2"), read_resource(p, "df"))
})

test_that("add_resource() sets correct properties for CSV resources", {
  p <- create_package()
  path <- system.file("extdata", "deployments.csv", package = "frictionless")

  # Encoding UTF-8 (0.8), ISO-8859-1 (0.59), ISO-8859-2 (0.26)
  p <- add_resource(p, "deployments", path)
  expect_identical(p$resources[[1]]$format, "csv")
  expect_identical(p$resources[[1]]$mediatype, "text/csv")
  expect_identical(p$resources[[1]]$encoding, "UTF-8")

  # Encoding ISO-8859-1 (0.6), ISO-8859-1 (0.26)
  p <- add_resource(p, "deployments_encoding",
                    test_path("data/deployments_encoding.csv"))
  expect_identical(p$resources[[2]]$format, "csv")
  expect_identical(p$resources[[2]]$mediatype, "text/csv")
  expect_identical(p$resources[[2]]$encoding, "ISO-8859-1")
  expect_identical(
    read_resource(p, "deployments_encoding"), # read_resource understands encod.
    read_resource(p, "deployments")
  )

  # Encoding UTF-8 (0.8), ISO-8859-1 (0.59), ISO-8859-2 (0.26), zip compressed
  p <- add_resource(p, "deployments_zip", test_path("data/deployments.csv.zip"))
  expect_identical(p$resources[[3]]$format, "csv") # .zip extension ignored
  expect_identical(p$resources[[3]]$mediatype, "text/csv")
  expect_identical(p$resources[[3]]$encoding, "UTF-8")
  expect_identical(
    read_resource(p, "deployments_zip"),
    read_resource(p, "deployments")
  )

  # Encoding ASCII, delimiter ","
  p <- add_resource(p, "df", test_path("data/df.csv"))
  expect_identical(p$resources[[4]]$format, "csv")
  expect_identical(p$resources[[4]]$mediatype, "text/csv")
  expect_identical(p$resources[[4]]$encoding, "UTF-8") # ASCII is set to UTF-8

  # Encoding ASCII, delimiter ";", extension "txt"
  p <- add_resource(p, "df_delim_1", test_path("data/df_delim_1.txt"),
                    delim = ";")
  expect_identical(p$resources[[5]]$format, "csv")
  expect_identical(p$resources[[5]]$mediatype, "text/csv")
  expect_identical(p$resources[[5]]$encoding, "UTF-8")
  expect_identical(read_resource(p, "df_delim_1"), read_resource(p, "df"))

  # Encoding ASCII, delimiter "\t", extension "tsv"
  p <- add_resource(p, "df_delim_2", test_path("data/df_delim_2.tsv"),
                    delim = "\t")
  expect_identical(p$resources[[6]]$format, "tsv")
  expect_identical(p$resources[[6]]$mediatype, "text/tab-separated-values")
  expect_identical(p$resources[[6]]$encoding, "UTF-8")
  expect_identical(read_resource(p, "df_delim_2"), read_resource(p, "df"))
})
