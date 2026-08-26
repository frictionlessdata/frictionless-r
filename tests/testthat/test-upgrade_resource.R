test_that("upgrade_descriptor() returns resource object", {
  resource_v1 <- resource(example_package(version = "1.0"), "deployments")
  # Resource object has "data_location" attribute that should be kept
  expect_identical(
    attr(upgrade_resource(resource_v1), "data_location"),
    attr(resource_v1, "data_location")
  )
})

test_that("upgrade_resource() upgrades a v1 resource to v2", {
  resource_v1 <- resource(example_package(version = "1.0"), "deployments")
  expect_identical(version(upgrade_resource(resource_v1)), "2.0")
})

test_that("upgrade_resource() leaves a v2 resource as is", {
  resource_v2 <- resource(example_package(version = "2.0"), "deployments")
  expect_identical(upgrade_resource(resource_v2), resource_v2)
})

test_that("upgrade_resource() removes profile", {
  resource_v1 <- resource(example_package(version = "1.0"), "deployments")
  resource_v1$profile <- "data-resource"
  expect_null(upgrade_resource(resource_v1)$profile)
})

test_that("upgrade_resource() sets $schema", {
  resource_v1 <- resource(example_package(version = "1.0"), "deployments")
  v2_profile <- "https://datapackage.org/profiles/2.0/dataresource.json"

  # Ignore undefined
  resource_v1$profile <- NULL
  expect_identical(upgrade_resource(resource_v1)$`$schema`, v2_profile)

  # Ignore default value
  resource_v1$profile <- "data-resource"
  expect_identical(upgrade_resource(resource_v1)$`$schema`, v2_profile)

  # Ignore tabular-data-resource (retained in type: table)
  resource_v1$profile <- "tabular-data-resource"
  expect_identical(upgrade_resource(resource_v1)$`$schema`, v2_profile)

  # Ignore URL to custom profile (hardly used)
  resource_v1$profile <- "http://example.com/my-profiles-json-schema.json"
  expect_identical(upgrade_resource(resource_v1)$`$schema`, v2_profile)

  # Ignore unregistered value
  resource_v1$profile <- "unregistered-resource"
  expect_identical(upgrade_resource(resource_v1)$`$schema`, v2_profile)
})

test_that("upgrade_resource() sets type for tabular resources", {
  resource_v1 <- resource(example_package(version = "1.0"), "deployments")

  # Set type for tabular-data-resource
  resource_v1$profile <- "tabular-data-resource"
  expect_identical(upgrade_resource(resource_v1)$type, "table")

  # Set type for tabular-data-resource profile URL
  resource_v1$profile <-
    "https://specs.frictionlessdata.io/schemas/tabular-data-resource.json"
  expect_identical(upgrade_resource(resource_v1)$type, "table")

  # Don't set type otherwise
  resource_v1$profile <- NULL
  expect_null(upgrade_resource(resource_v1)$type)
  resource_v1$profile <- "data-resource"
  expect_null(upgrade_resource(resource_v1)$type)
  resource_v1$profile <- "http://example.com/my-profiles-json-schema.json"
  expect_null(upgrade_resource(resource_v1)$type)
})
