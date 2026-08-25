test_that("resource() can upgrade a resource from v1 to v2", {
  skip_if_offline()

  # Upgrade v1 if asked
  p_v1 <- example_package(version = "1.0")
  expect_identical(version(resource(p_v1, "deployments")), "1.0")
  expect_identical(version(resource(p_v1, "deployments", upgrade = TRUE)), "2.0")

  # Leave v2 as is
  p_v2 <- example_package(version = "2.0")
  expect_identical(version(resource(p_v2, "deployments")), "2.0")
  expect_identical(version(resource(p_v2, "deployments", upgrade = TRUE)), "2.0")
})

test_that("resource() returns resource (in same version) as provided", {
  skip_if_offline()

  p_v1 <- example_package(version = "1.0")
  expect_identical(
    version(resource(p_v1, "deployments")),
    version(p_v1)
  )

  p_v2 <- example_package(version = "2.0")
  expect_identical(
    version(resource(p_v2, "deployments")),
    version(p_v2)
  )
})

test_that("resource()<- returns schema (in same version) as provided", {
  skip_if_offline()

  p_v1_assign <- p_v1 <- example_package(version = "1.0")
  resource(p_v1_assign, "deployments")$name <- "custom name"

  expect_identical(
    version(resource(p_v1, "deployments")),
    version(resource(p_v1_assign, "custom name"))
  )

  p_v2_assign <- p_v2 <- example_package(version = "2.0")
  resource(p_v2_assign, "deployments")$name <- "custom name"

  expect_identical(
    version(resource(p_v2, "deployments")),
    version(resource(p_v2_assign, "custom name"))
  )
})

test_that("resource()<- returns a Data Package invisibly", {
  skip_if_offline()
  p <- example_package()
  expect_invisible(resource(p, "deployments") <- list(name = "deployments"))
})

test_that("resource()<- returns error on invalid Data Package", {
  p_invalid <- list()
  expect_error(
    resource(p_invalid, "assigned") <- list(name = "assigned"),
    class = "frictionless_error_package_invalid"
  )
})

test_that("resource()<- returns error when resource not found", {
  skip_if_offline()
  p <- example_package()
  expect_error(
    resource(p, "no_such_resource") <- list(name = "assigned"),
    class = "frictionless_error_resource_not_found"
  )
})

test_that("resource()<- allows overwriting a resource", {
  skip_if_offline()
  p <- example_package()
  resource <- resource(p, "deployments")
  resource$title <- "Custom title"
  resource(p, "deployments") <- resource
  expect_identical(p$resources[[1]]$title, "Custom title")
})

test_that("resource()$property<- allows overwriting a resource property", {
  skip_if_offline()
  p <- example_package()
  resource(p, "deployments")$title <- "Custom title"
  expect_identical(p$resources[[1]]$title, "Custom title")
})

test_that("resource()<- allows overwriting with NULL, removing the resource", {
  skip_if_offline()
  p <- p_assigned <- example_package()
  resource(p_assigned, "deployments") <- NULL
  expect_identical(
    p_assigned,
    remove_resource(p, "deployments")
  )
})

test_that("resource()<- allows overwriting a resource with a different name", {
  skip_if_offline()
  p <- example_package()
  resource(p, "deployments") <- list(name = "not_deployments")
  expect_identical(
    resource_names(p),
    c("not_deployments", "observations", "media")
  )
})

test_that("resource()<- reverts changes made by resource()", {
  skip_if_offline()
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  df_csv <- test_path("data/df.csv")
  df_url <- file.path(
    "https://raw.githubusercontent.com/frictionlessdata/frictionless-r",
    "main/tests/testthat/data/df.csv"
  )

  # Create package with mix of existing and new resources
  p <-
    example_package() |>
    add_resource("df", df) |>
    add_resource("csv", df_csv) |>
    add_resource("csv_multiple", c(df_csv, df_csv)) |>
    add_resource("url", df_url) |>
    add_resource("url_multiple", c(df_url, df_url))

  # Assign each resource with its resource() equivalent
  p_assigned <- p
  resource(p_assigned, "deployments") <- resource(p_assigned, "deployments")
  resource(p_assigned, "observations") <- resource(p_assigned, "observations")
  resource(p_assigned, "media") <- resource(p_assigned, "media")
  resource(p_assigned, "df") <- resource(p_assigned, "df")
  resource(p_assigned, "csv") <- resource(p_assigned, "csv")
  resource(p_assigned, "csv_multiple") <- resource(p_assigned, "csv_multiple")
  resource(p_assigned, "url") <- resource(p_assigned, "url")
  resource(p_assigned, "url_multiple") <- resource(p_assigned, "url_multiple")

  # Expect unchanged package
  expect_identical(p_assigned, p)
})
