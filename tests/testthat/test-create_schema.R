test_that("create_schema() returns a valid Table Schema", {
  df <- data.frame(
    "col_1" = c(1, 2),
    "col_2" = factor(c("a", "b"), levels = c("a", "b", "c"))
  )
  expected_schema <- list(
    fields = list(
      list(
        name = "col_1",
        type = "number"
        # Do not add empty property "constraints"
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
  expect_identical(create_schema(df), expected_schema)
  expect_true(check_schema(create_schema(df)))
})

test_that("create_schema() returns error on invalid or empty data frame", {
  expect_error(
    create_schema("not_a_df"),
    "`data` must be a data frame containing data.",
    fixed = TRUE
  )
  expect_error(
    create_schema(data.frame()),
    "`data` must be a data frame containing data.",
    fixed = TRUE
  )
})

test_that("create_schema() accepts data frames and tibbles", {
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  tibble <- dplyr::tibble("col_1" = c(1, 2), "col_2" = c("a", "b"))
  expect_type(create_schema(df), "list")
  expect_type(create_schema(tibble), "list")
})

test_that("create_schema() uses colnames as field names", {
  df <- data.frame(NA, NA, NA, NA)
  colnames <- c("col_1", "Column 2", "col_3!") # Only 3 of 4 defined
  colnames(df) <- colnames
  expect_identical(
    purrr::map_chr(create_schema(df)$fields, ~ .x$name),
    c(colnames, "") # Last unnamed column (NA) should have empty name ""
  )
})

test_that("create_schema() translates coltypes into field types", {
  interval <- lubridate::interval(
    lubridate::ymd("2020-03-01"), lubridate::ymd("2020-03-02")
  )
  dttm <- "2020-03-01T08:00:00"

  # Create data.frame with base classes + some returned by read_delim()
  df <- data.frame(
    array = as.array(1),                # numeric
    character = as.character(1),        # character
    complex = as.complex(1),            # complex
    # data.frame = as.data.frame(1),    # results in X1 (numeric)
    Date = as.Date("2020-03-01"),       # Date
    difftime = as.difftime(interval, units = "weeks"), # difftime
    # dist = as.dist(1),                # cannot coerce to df
    double = as.double(1),              # numeric
    factor = as.factor(1),              # factor
    hms = hms::as_hms("08:00:00"),      # hms,difftime
    integer = as.integer(1),            # integer
    # list = as.list(1),                # results in X1 (numeric)
    logical = as.logical(1),            # logical
    matrix = as.matrix(1),              # numeric
    # null = as.null(),                 # args imply differing number of rows
    numeric = as.numeric(1),            # numeric
    # octmode = as.octmode(1),          # cannot coerce to df
    period = lubridate::as.period(interval), # Period
    # person = as.person(1),            # cannot coerce to df
    POSIXct = as.POSIXct(dttm),         # POSIXct,POSIXt
    POSIXct_tz = as.POSIXct(dttm, tz = "EET"), # POSIXct,POSIXt
    POSIXlt = as.POSIXlt(dttm),         # POSIXct,POSIXt
    POSIXlt_tz = as.POSIXlt(dttm, tz = "EET"), # POSIXct,POSIXt
    # raster = as.raster(1),            # cannot coerce to df
    raw = as.raw(1),                    # raw
    # roman = as.roman(1),              # cannot coerce to df
    single = as.single(1),              # numeric
    # symbol = as.symbol(1),            # cannot coerce to df
    # table = as.table(1),              # results in table.Var1 (fct), table.Freq (num)
    ts = as.ts(1),                      # ts
    vector = as.vector(1)               # numeric
  )
  schema <- create_schema(df)
  types <- purrr::map(schema$fields, ~ .x$type)
  types <- setNames(types, purrr::map_chr(schema$fields, ~ .x$name))

  expect_identical(
    types,
    list(
      array = "number",
      character = "string",
      complex = "any",
      Date = "date",
      difftime = "number", # Expressed as number when written to csv
      double = "number",
      factor = "string",
      hms = "time",
      integer = "integer",
      logical = "boolean",
      matrix = "number",
      numeric = "number",
      period = "any", # 1d 0H 0M 0S
      POSIXct = "datetime",
      POSIXct_tz = "datetime",
      POSIXlt = "datetime",
      POSIXlt_tz = "datetime",
      raw = "any",
      single = "number",
      ts = "any",
      vector = "number"
    )
  )
})
