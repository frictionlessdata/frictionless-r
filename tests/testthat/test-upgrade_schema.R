# Return ----
test_that("upgrade_schema() returns a valid schema", {
  schema_v1 <- schema(example_package(version = "1.0"), "deployments")
  expect_no_error(check_schema(upgrade_schema(schema_v1, "deployments")))
})

# Functionality ----
test_that("upgrade_schema() upgrades a v1 schema to v2", {
  schema_v1 <- schema(example_package(version = "1.0"), "deployments")
  expect_identical(version(upgrade_schema(schema_v1, "deployments")), "2.0")
})

test_that("upgrade_schema() leaves a v2 schema as is", {
  schema_v2 <- schema(example_package(version = "2.0"), "deployments")
  expect_identical(upgrade_schema(schema_v2, "deployments"), schema_v2)
})

test_that("upgrade_schema() sets $schema to default v2 value", {
  schema_v1 <- schema(example_package(version = "1.0"), "deployments")
  expect_identical(
    upgrade_schema(schema_v1, "deployments")$`$schema`,
    "https://datapackage.org/profiles/2.0/tableschema.json"
    )
})

test_that("upgrade_schema() updates primaryKey", {
  schema_v1 <- schema(example_package(version = "1.0"), "deployments")

  # Example v1 becomes v2
  expect_identical(
    upgrade_schema(schema_v1, "deployments")$primaryKey,
    schema(example_package(version = "2.0"), "deployments")$primaryKey,
  )

  # Leave if undefined
  schema_v1$primaryKey <- NULL
  expect_null(upgrade_schema(schema_v1, "deployments")$primaryKey)

  # Update if character
  schema_v1$primaryKey <- "deployment_id"
  expect_identical(
    upgrade_schema(schema_v1, "deployments")$primaryKey,
    list("deployment_id")
  )

  # Leave if list
  schema_v1$primaryKey <- list("deployment_id", "other_id")
  expect_identical(
    upgrade_schema(schema_v1, "deployments")$primaryKey,
    list("deployment_id", "other_id")
  )
})

test_that("upgrade_schema() updates foreignKeys", {
  schema_v1 <- schema(example_package(version = "1.0"), "observations")

  # observations resource, primaryKey and foreignKeys
  schema_v1 <- schema(example_package(version = "1.0"), "observations")
  schema_v2 <- schema(example_package(version = "2.0"), "observations")
  schema_upgraded <- upgrade_schema(schema_v1, resource_name = "observations")
  expect_identical(schema_upgraded$foreignKeys, schema_v2$foreignKeys)

  # Remove schema.foreignKeys.reference[x].resource if equal to resource name
  schema_v1$foreignKeys[[1]]$reference$resource <- "media"
  schema_upgraded <- upgrade_schema(schema_v1, resource_name = "media")
  expect_null(schema_upgraded$foreignKeys[[1]]$reference$resource)
})
