test_that("upgrade_resource() upgrades a resource v1 to v2", {
  resource_v1 <- resource(example_package(version = "1.0"), "deployments")
  expect_identical(version(upgrade_resource(resource_v1)), "2.0")
})

test_that("upgrade_resource() leaves a v2 resource as is", {
  resource_v2 <- resource(example_package(version = "2.0"), "deployments")
  expect_identical(upgrade_resource(resource_v2), resource_v2)
})

test_that("upgrade_resource() sets $schema", {
  resource_v1 <- resource(example_package(version = "1.0"), "deployments")
  v2_profile <- "https://datapackage.org/profiles/2.0/dataresource.json"

  # $schema is always set to v2 default, irrespective of profile
  # Undefined
  resource_v1$profile <- NULL
  expect_identical(upgrade_resource(resource_v1)$`$schema`, v2_profile)

  # Default value
  resource_v1$profile <- "data-resource"
  expect_identical(upgrade_resource(resource_v1)$`$schema`, v2_profile)

  # Tabular resource (retained in type: table)
  resource_v1$profile <- "tabular-data-resource"
  expect_identical(upgrade_resource(resource_v1)$`$schema`, v2_profile)

  # URL to custom profile (hardly used, so ignored)
  resource_v1$profile <- "http://example.com/my-profiles-json-schema.json"
  expect_identical(upgrade_resource(resource_v1)$`$schema`, v2_profile)

  # Unregistered value
  resource_v1$profile <- "unregistered-resource"
  expect_identical(upgrade_resource(resource_v1)$`$schema`, v2_profile)
})

test_that("upgrade_resource() sets type for tabular resources", {
  resource_v1 <- resource(example_package(version = "1.0"), "deployments")

  # Tabular resource
  resource_v1$profile <- "tabular-data-resource"
  expect_identical(upgrade_resource(resource_v1)$type, "table")

  # Tabular resource profile URL
  resource_v1$profile <-
    "https://specs.frictionlessdata.io/schemas/tabular-data-resource.json"
  expect_identical(upgrade_resource(resource_v1)$type, "table")

  # Undefined
  resource_v1$profile <- NULL
  expect_null(upgrade_resource(resource_v1)$type)
})

test_that("upgrade_resource() sets removes profile", {
  resource_v1 <- resource(example_package(version = "1.0"), "deployments")

  resource_v1$profile <- "data-resource"
  expect_null(upgrade_resource(resource_v1)$profile)
})
