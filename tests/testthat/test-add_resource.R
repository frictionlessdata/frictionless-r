test_that("add_resource() returns a valid package", {
  p <- example_package()
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  df_csv <- test_path("data/df.csv")
  schema <- create_schema(df)
  expect_no_error(check_package(add_resource(p, "new", df)))
  expect_no_error(check_package(add_resource(p, "new", df, schema)))
  expect_no_error(check_package(add_resource(p, "new", df_csv)))
  expect_no_error(check_package(
    add_resource(p, "new", df, title = "New", foo = "bar")
  ))
})

test_that("add_resource() returns package in same version as provided", {
  p_v1 <- example_package(version = "1.0")
  p_v2 <- example_package(version = "2.0")
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  expect_identical(version(add_resource(p_v1, "new", df)), "1.0")
  expect_identical(version(add_resource(p_v2, "new", df)), "2.0")
})

test_that("add_resource() returns error on package", {
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  expect_error(
    add_resource(list(), "new", df),
    class = "frictionless_error_package_invalid"
  )
})

test_that("add_resource() allows any string as resource name and trims it", {
  p <- example_package()
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))

   expect_no_error(
    add_resource(p, "  Nëw  re/source 4 ", df)
  )
  expect_contains(
    resource_names(add_resource(p, "  Nëw  re/source 4 ", df)),
    "Nëw  re/source 4" # Trimmed
  )
})

test_that("add_resource() returns error when replace is not a logical value", {
  p <- example_package()
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  expect_error(
    add_resource(p, "new_resource", df, replace = "not_a_logical"),
    class = "frictionless_error_replace_invalid"
  )
  expect_no_error(
    check_package(add_resource(p, "new_resource", df, replace = TRUE))
  )
  expect_no_error(
    check_package(add_resource(p, "new_resource", df, replace = FALSE))
  )
})

test_that("add_resource() returns error when resource of that name already
           exists (for default replace = FALSE)", {
  p <- example_package()
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  expect_error(
    add_resource(p, "deployments", df),
    class = "frictionless_error_resource_already_exists"
  )
  expect_error(
    add_resource(p, "deployments", df),
    regexp = "`package` already contains a resource named \"deployments\".",
    fixed = TRUE
  )
  expect_error(
    add_resource(p, "deployments", df),
    regexp = "Use `replace = TRUE` to replace an existing resource.",
    fixed = TRUE
  )
})

test_that("add_resource() returns error when data is not data frame or
           character", {
  p <- example_package()
  expect_error(
    add_resource(p, "new", list()),
    class = "frictionless_error_data_type_invalid"
  )
  expect_error(
    add_resource(p, "new", list()),
    regexp = "`data` must either be a data frame or path(s) to CSV file(s).",
    fixed = TRUE
  )
})

test_that("add_resource() returns error on invalid or empty data frame", {
  p <- example_package()
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  schema <- create_schema(df)
  expect_error(
    add_resource(p, "new", data.frame("col_1" = character(0))),
    class = "frictionless_error_data_invalid"
  )
  expect_error(
    add_resource(p, "new", data.frame("col_1" = character(0)), schema),
    class = "frictionless_error_data_invalid"
  )

  # For more tests see test-check_schema.R
})

test_that("add_resource() returns error if CSV file cannot be found", {
  skip_if_offline()
  p <- example_package()
  df_csv <- test_path("data/df.csv")
  schema <- create_schema(data.frame("col_1" = c(1, 2), "col_2" = c("a", "b")))
  expect_error(
    add_resource(p, "new", "no_such_file.csv"),
    class = "frictionless_error_path_not_found"
  )
  expect_error(
    add_resource(p, "new", "no_such_file.csv", schema),
    class = "frictionless_error_path_not_found"
  )
  expect_error(
    add_resource(p, "new", c(df_csv, "no_such_file.csv")),
    class = "frictionless_error_path_not_found"
  )
  expect_error(
    add_resource(p, "new", c("no_such_file.csv", df_csv)),
    class = "frictionless_error_path_not_found"
  )
  expect_error(
    add_resource(p, "new", c("no_such_file_1.csv", "no_such_file_2.csv")),
    class = "frictionless_error_path_not_found"
  )
  expect_error(
    add_resource(p, "new", "https://example.com/no_such_file.csv"),
    class = "frictionless_error_url_not_found"
  )
})

test_that("add_resource() returns error on mismatching schema and data", {
  p <- example_package()
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  df_csv <- test_path("data/df.csv")
  schema_invalid <- create_schema(df) # Not yet invalid
  schema_invalid$fields[[1]]$name <- "no_such_col"

  # df
  expect_error(
    add_resource(p, "new", df, schema_invalid),
    class = "frictionless_error_fields_colnames_mismatch"
  )

  # csv
  expect_error(
    add_resource(p, "new", df_csv, schema_invalid),
    class = "frictionless_error_fields_colnames_mismatch"
  )

  # For more tests see test-check_schema.R
})

test_that("add_resource() returns error if ... arguments are unnamed", {
  p <- create_package()
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  schema <- create_schema(df)
  expect_error(
    add_resource(p, "new", df, schema, replace = FALSE, delim = ",", "unnamed"),
    class = "frictionless_error_argument_unnamed"
  )
  expect_error(
    add_resource(p, "new", df, schema, replace = FALSE, delim = ",", "unnamed"),
    "All arguments in `...` must be named.",
    fixed = TRUE
  )
})

test_that("add_resource() returns error if ... arguments are reserved", {
  p <- create_package()
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  expect_error(
    add_resource(p, "new", df, name = "custom_name"),
    class = "frictionless_error_resource_properties_reserved"
  )
  expect_error(
    add_resource(p, "new", df, name = "custom_name", format = "custom_format"),
    class = "frictionless_error_resource_properties_reserved"
  )

  expect_error(
    add_resource(p, "new", df, name = "custom_name"),
    regexp = "`name` must be removed as argument.",
    fixed = TRUE
  )
  expect_error(
    add_resource(p, "new", df, name = "custom_name"),
    regexp = "name is automatically added as resource property.",
    fixed = TRUE
  )
  expect_error(
    add_resource(p, "new", df, name = "custom_name", format = "custom_format"),
    regexp = "`name` and `format` must be removed as arguments.",
    fixed = TRUE
  )
  expect_error(
    add_resource(p, "new", df, name = "custom_name", format = "custom_format"),
    regexp = "name and format are automatically added as resource properties.",
    fixed = TRUE
  )
})

test_that("add_resource() adds resource", {
  p <- example_package()
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  df_csv <- test_path("data/df.csv")

  # df
  p <- add_resource(p, "new_df", df)
  expect_length(p$resources, 4) # Remains a list, now of length 4
  expect_identical(
    p$resources[[4]][["$schema"]],
    "https://datapackage.org/profiles/2.0/dataresource.json"
  )
  expect_identical(p$resources[[4]][["name"]], "new_df")
  expect_identical(p$resources[[4]][["type"]], "table")
  expect_identical(p$resources[[4]][["data"]], df)
  expect_identical(
    resource_names(p),
    c("deployments", "observations", "media", "new_df")
  )

  # csv
  p <- add_resource(p, "new_csv", df_csv)
  expect_length(p$resources, 5) # Remains a list, now of length 5
  expect_identical(
    p$resources[[5]][["$schema"]],
    "https://datapackage.org/profiles/2.0/dataresource.json"
  )
  expect_identical(p$resources[[5]][["name"]], "new_csv")
  expect_identical(p$resources[[5]][["type"]], "table")
  expect_null(p$resources[[5]][["data"]])
  expect_identical(
    resource_names(p),
    c("deployments", "observations", "media", "new_df", "new_csv")
  )
})

test_that("add_resource() adds a v2 resource", {
  p_v1 <- example_package(version = "1.0")
  p_v2 <- example_package(version = "2.0")
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  p_v1 <- add_resource(p_v1, "new", df)
  p_v2 <- add_resource(p_v2, "new", df)

  expect_identical(version(resource(p_v1, "new")), "2.0")
  expect_identical(version(resource(p_v2, "new")), "2.0")
})

test_that("add_resource() can replace an existing resource", {
  p <- example_package()
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  expect_no_error(
    add_resource(p, "deployments", df, replace = TRUE)
  )
  p_replaced <- add_resource(p, "deployments", df, replace = TRUE)
  expect_identical(resource_names(p), resource_names(p_replaced))
})

test_that("add_resource() can add a new resource even with replace = TRUE", {
  p <- example_package()
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  expect_no_error(
    add_resource(p, "new_resource", df, replace = TRUE)
  )
  p_replaced <- add_resource(p, "new_resource", df, replace = TRUE)
  expect_identical(c(resource_names(p), "new_resource"), resource_names(p_replaced))
})

test_that("adds_resource() sets resource$schema", {
  p <- example_package()

  # Add new resource
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  p_new <- add_resource(p, "new_resource", df)
  expect_identical(
    resource(p_new, "new_resource")$`$schema`,
    "https://datapackage.org/profiles/2.0/dataresource.json"
  )

  # Replace resource
  deployments <- system.file(
    "extdata", "v2", "deployments.csv", package = "frictionless"
  )
  expect_null(resource(p, "deployments")$`$schema`)

  p_replaced <- add_resource(p, "deployments", deployments, replace = TRUE)
  expect_identical(
    resource(p_replaced, "deployments")$`$schema`,
    "https://datapackage.org/profiles/2.0/dataresource.json"
  )
})

test_that("add_resource() uses provided schema (list or path) or creates one", {
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
  expect_identical(schema(p, "new_df"), schema)
  expect_identical(schema(p, "new_df_with_list_schema"), schema_custom)
  expect_identical(schema(p, "new_df_with_file_schema"), schema_custom)

  # csv
  p <- add_resource(p, "new_csv", df)
  p <- add_resource(p, "new_csv_with_list_schema", df, schema_custom)
  p <- add_resource(p, "new_csv_with_file_schema", df, schema_file)
  expect_identical(p$resources[[4]]$schema, schema)
  expect_identical(p$resources[[5]]$schema, schema_custom)
  expect_identical(p$resources[[6]]$schema, schema_custom)
  expect_identical(schema(p, "new_csv"), schema)
  expect_identical(schema(p, "new_csv_with_list_schema"), schema_custom)
  expect_identical(schema(p, "new_csv_with_file_schema"), schema_custom)
})

test_that("add_resource() keeps URL schema as URL", {
  skip_if_offline()
  p <- example_package()
  deployments <- read_resource(p, "deployments")
  schema_url <- file.path(
    "https://raw.githubusercontent.com/frictionlessdata/frictionless-r",
    "main/tests/testthat/data/deployments_schema.json"
  )
  p <- add_resource(p, "deployments", deployments, schema_url, replace = TRUE)
  expect_identical(p$resources[[1]]$schema, schema_url)
})

test_that("add_resource() can add resource from data frame, readable by
           read_resource()", {
  p <- example_package()
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  p <- add_resource(p, "new", df)
  expect_identical(read_resource(p, "new"), dplyr::as_tibble(df))
})

test_that("add_resource() can add resource from local, relative, absolute,
           remote or compressed CSV file, readable by read_resource()", {
  skip_if_offline()
  p <- example_package()
  schema <- schema(p, "deployments")

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
    "extdata", "v1", "deployments.csv", package = "frictionless" # Will start with /
  )
  p <- add_resource(p, "absolute", absolute_path, schema)
  expect_identical(p$resources[[6]]$path, absolute_path)
  expect_s3_class(read_resource(p, "absolute"), "tbl")

  # Remote
  remote_path <- file.path(
    "https://raw.githubusercontent.com/frictionlessdata/frictionless-r",
    "main/inst/extdata/v1/deployments.csv"
  )
  p <- add_resource(p, "remote", remote_path, schema)
  expect_identical(p$resources[[7]]$path, remote_path)
  expect_s3_class(read_resource(p, "remote"), "tbl")

  # Compressed
  compressed_file <- test_path("data/deployments.csv.gz")
  p <- add_resource(p, "compressed", compressed_file, schema)
  expect_identical(p$resources[[8]]$path, compressed_file)
  expect_s3_class(read_resource(p, "compressed"), "tbl")
})

test_that("add_resource() can add resource from CSV file with other delimiter,
           readable by read_resource()", {
  p <- create_package()
  p <- add_resource(p, "df", test_path("data/df.csv"))
  expect_null(p$resources[[1]]$dialect$delimiter)
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
  path <- system.file("extdata", "v1", "deployments.csv", package = "frictionless")

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

test_that("add_resource() sets ... arguments as extra properties", {
  p <- create_package()
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  df_csv <- test_path("data/df.csv")

  # df
  p <- add_resource(p, "new_df", df, title = "custom_title", foo = "bar")
  expect_identical(p$resources[[1]]$title, "custom_title")
  expect_identical(p$resources[[1]]$foo, "bar")

  # csv
  p <- add_resource(p, "new_csv", df_csv, title = "custom_title", foo = "bar")
  expect_identical(p$resources[[2]]$title, "custom_title")
  expect_identical(p$resources[[2]]$foo, "bar")
})
