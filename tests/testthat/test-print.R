test_that("print() returns output invisibly", {
  expect_output(output <- withVisible(print(example_package)))
  expect_false(output$visible)
})

test_that("print() informs about the resources and unclass()", {
  unclass_message <- "Use `unclass()` to print the Data Package as a list."

  # 3 resources (example package)
  p <- example_package
  expect_output(
    print(p),
    regexp = paste(
      "A Data Package with 3 resources:",
      "• deployments",
      "• observations",
      "• media",
      unclass_message,
      sep = "\n"
    ),
    fixed = TRUE
  )

  # 1 resource
  p1 <- create_package()
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  p1 <- add_resource(p1, "new", df)
  expect_output(
    print(p1),
    regexp = paste(
      "A Data Package with 1 resource:",
      "• new",
      unclass_message,
      sep = "\n"
    ),
    fixed = TRUE
  )

  # 0 resources
  p0 <- create_package()
  expect_output(
    print(p0),
    regexp = paste(
      "A Data Package with 0 resources.",
      unclass_message,
      sep = "\n"
    ),
    fixed = TRUE
  )
})

testthat("print() informs about more information in package$id", {
  unclass_message <- "Use `unclass()` to print the Data Package as a list."

  # package$id is a URL, inform
  p <- create_package()
  p$id <- "https://example.com"
  expect_output(
    print(p),
    regexp = paste(
      "A Data Package with 0 resources.",
      "For more information, see <https://example.com>.",
      unclass_message,
      sep = "\n"
    ),
    fixed = TRUE
  )

  # package$id is not a URL, don't inform
  p$id <- "not_a_url"
  expect_output(
    print(p),
    regexp = paste(
      "A Data Package with 0 resources.",
      unclass_message,
      sep = "\n"
    ),
    fixed = TRUE
  )
})
