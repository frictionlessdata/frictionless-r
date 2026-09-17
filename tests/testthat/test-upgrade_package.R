test_that("upgrade_package() upgrades a package, its resources and schemas to
           v2", {
  p_v1 <- example_package(version = "1.0")
  p_upgraded <- upgrade_package(p_v1)

  expect_identical(version(p_upgraded), "2.0")
  expect_identical(version(resource(p_upgraded, "deployments")), "2.0")
  expect_identical(version(resource(p_upgraded, "observations")), "2.0")
  expect_identical(version(resource(p_upgraded, "media")), "2.0")
  expect_identical(version(schema(p_upgraded, "deployments")), "2.0")
  expect_identical(version(schema(p_upgraded, "observations")), "2.0")
  expect_identical(version(schema(p_upgraded, "media")), "2.0")
})

test_that("upgrade_package() does not upgrade non-verbose schemas", {
  skip_if_offline()
  p_v1 <- example_package(version = "1.0")
  p_v1 <- remove_resource(p_v1, "observations")
  p_v1 <- remove_resource(p_v1, "media")
  attr(p_v1, "directory") <- "."

  # Use a remote path, otherwise schema and path need to share same directory
  p_v1$resources[[1]]$path <- file.path(
    "https://raw.githubusercontent.com/frictionlessdata/frictionless-r",
    "main/inst/extdata/v1/deployments.csv"
  )

  # Schema is local path
  p_v1_local_schema <- p_v1
  p_v1_local_schema$resources[[1]]$schema <-
    test_path("data/deployments_schema_v1.json")
  expect_identical(
    version(schema(upgrade_package(p_v1_local_schema), "deployments")),
    "1.0"
  )

  # Schema is remote path
  p_v1_remote_schema <- p_v1
  p_v1_remote_schema$resources[[1]]$schema <- file.path(
    "https://raw.githubusercontent.com/frictionlessdata/frictionless-r",
    "main/tests/testthat/data/deployments_schema_v1.json"
  )
  expect_identical(
    version(schema(upgrade_package(p_v1_remote_schema), "deployments")),
    "1.0"
  )
})

test_that("upgrade_package() harmonizes mixed versions", {
  p_v1 <- example_package(version = "1.0")
  # Upgrade package, but not its resources
  p_mixed <- upgrade_descriptor(p_v1)
  # Add v2 schema to v1 resource
  deployments <- read_resource(p_mixed, "deployments")
  p_mixed$resources[[1]]$schema <- create_schema(deployments)
  # Add v2 resource with v2 schema
  df <- data.frame("col_1" = c(1, 2), "col_2" = c("a", "b"))
  p_mixed <- add_resource(p_mixed, "new_df", df)

  # Package with mixed versions
  expect_identical(version(p_mixed), "2.0")
  expect_identical(version(resource(p_mixed, "deployments")), "1.0")
  expect_identical(version(resource(p_mixed, "observations")), "1.0")
  expect_identical(version(resource(p_mixed, "new_df")), "2.0")
  expect_identical(version(schema(p_mixed, "deployments")), "2.0")
  expect_identical(version(schema(p_mixed, "observations")), "1.0")
  expect_identical(version(schema(p_mixed, "new_df")), "2.0")

  # Upgrade package has harmonized versions
  p_upgraded <- upgrade_package(p_mixed)
  expect_identical(version(p_upgraded), "2.0")
  expect_identical(version(resource(p_upgraded, "deployments")), "2.0")
  expect_identical(version(resource(p_upgraded, "observations")), "2.0")
  expect_identical(version(resource(p_upgraded, "new_df")), "2.0")
  expect_identical(version(schema(p_upgraded, "deployments")), "2.0")
  expect_identical(version(schema(p_upgraded, "observations")), "2.0")
  expect_identical(version(schema(p_upgraded, "new_df")), "2.0")
})
