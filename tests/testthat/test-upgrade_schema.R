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

  # Update character
  schema_v1$primaryKey <- "deployment_id"
  expect_identical(
    upgrade_schema(schema_v1, "deployments")$primaryKey,
    list("deployment_id")
  )

  # Leave undefined
  schema_v1$primaryKey <- NULL
  expect_null(upgrade_schema(schema_v1, "deployments")$primaryKey)

  # Leave list
  schema_v1$primaryKey <- list("deployment_id", "other_id")
  expect_identical(
    upgrade_schema(schema_v1, "deployments")$primaryKey,
    list("deployment_id", "other_id")
  )
})

test_that("upgrade_schema() updates foreignKeys", {
  schema_v1 <- schema(example_package(version = "1.0"), "media")
  # Media has fk to deployment.deployment_id and observations.observation_id

  # v1 foreignKeys become v2
  expect_identical(
    upgrade_schema(schema_v1, "media")$foreignKeys,
    schema(example_package(version = "2.0"), "media")$foreignKeys,
  )

  # Leave undefined
  schema_v1$foreignKeys <- NULL
  expect_null(upgrade_schema(schema_v1, "media")$foreignKeys)

  # Extensive example
  provided_keys <- list(
    fields = "not_in_list", # Ignore
    list(
      fields = "media_id", # Update to list
      custom = "custom", # Ignore
      reference = list(
        resource = "media", # Self-referential, remove
        fields = "media_id", # Update to list
        custom = "custom" # Ignore
      )
    ),
    list(
      custom = "custom", # Ignore
      reference = list(custom = "custom") # Ignore
    )
  )
  expected_keys <- list(
    fields = "not_in_list",
    list(
      fields = list("media_id"),
      custom = "custom",
      reference = list(
        fields = list("media_id"),
        custom = "custom"
      )
    ),
    list(
      custom = "custom",
      reference = list(custom = "custom")
    )
  )
  schema_v1$foreignKeys <- provided_keys
  expect_identical(
    upgrade_schema(schema_v1, "media")$foreignKeys,
    expected_keys
  )
})
