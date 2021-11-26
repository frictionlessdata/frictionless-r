test_that("create_schema() returns error on incorrect df", {
  expect_error(create_schema("not_a_df"), "`df` must be a data frame.")
})

test_that("create_schema() accepts data frames and tibbles", {
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  tibble <- tibble("col_1" = c(1, 2), "col_2" = c("a", "b"))
  expect_type(create_schema(df), "list")
  expect_type(create_schema(tibble), "list")
})

test_that("create_schema() returns a Table Schema as a list without empty properties", {
  df <- data.frame(
    "col_1" = c(1, 2),
    "col_2" = factor(c("a", "b"), levels = c("a", "b", "c"))
  )
  expected_schema <- list(
    fields = list(
      list(
        name = "col_1",
        type = "number"
        # No constraints
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
  expect_equal(create_schema(df), expected_schema)
})

test_that("create_schema() uses colnames as field names", {
  df <- data.frame(NA, NA, NA, NA)
  colnames <- c("col_1", "Column 2", "col_3!") # Only 3 of 4 defined
  colnames(df) <- colnames
  expect_equal(
    map_chr(create_schema(df)$fields, ~ .x$name),
    c(colnames, "") # Last unnamed column (NA) should have empty name ""
  )
})

test_that("create_schema() translates coltypes into field types", {
  interval <- interval(ymd("2020-03-01"), ymd("2020-03-02"))
  dttm <- "2020-03-01T08:00:00"

  df <- data.frame(
    array = as.array(1),                # numeric
    character = as.character(1),        # character
    # data.frame = as.data.frame(1),    # results in X1 (numeric)
    Date = as.Date("2020-03-01"),       # Date
    difftime = as.difftime(interval, units = "weeks"), # difftime
    double = as.double(1),              # numeric
    duration = as.duration(interval),   # Duration
    factor = as.factor(1),              # factor
    hms = hms::as_hms("08:00:00"),      # hms,difftime
    hms2 = lubridate::hms("O8:00:00"),  # Period
    integer = as.integer(1),            # integer
    interval = as.interval(interval),   # Interval
    logical = as.logical(1),            # logical
    matrix = as.matrix(1),              # numeric
    # null = as.null(),                 # args imply differing number of rows
    numeric = as.numeric(1),            # numeric
    # octmode = as.octmode(1),          # cannot coerce to df
    period = as.period(interval),       # Period
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
    # table = as.table(1),              # results in table.Var1 (factor) and
                                        # table.Freq (numeric)
    ts = as.ts(1),                      # ts
    vector = as.vector(1)               # numeric
  )
  schema <- create_schema(df)
  types <- map(schema$fields, ~ .x$type)
  types <- setNames(types, map_chr(schema$fields, ~ .x$name))

  expect_equal(
    types,
    list(
      array = "number",
      character = "string",
      Date = "date",
      difftime = "number", # Expressed as number when written to csv
      double = "number",
      duration = "any", # 86400s (~1 days)
      factor = "string",
      hms = "time",
      hms2 = "any", # 8H 0M 0S
      integer = "integer",
      interval = "any", # 2020-03-01 UTC--2020-03-02 UTC
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
