# Functionality ----
## Package ----
test_that("version() returns correct version for package", {
  package <- create_package()

  # Undefined $schema
  package$`$schema` <- NULL
  expect_identical(version(package), "1.0")

  # Defined default
  package$`$schema` <- "https://datapackage.org/profiles/1.0/datapackage.json"
  expect_identical(version(package), "1.0")

  # 2.0
  package$`$schema` <- "https://datapackage.org/profiles/2.0/datapackage.json"
  expect_identical(version(package), "2.0")

  # Future versions
  package$`$schema` <- "https://datapackage.org/profiles/2.1/datapackage.json"
  expect_identical(version(package), "2.1")
  package$`$schema` <- "https://datapackage.org/profiles/2.2-rc.1/datapackage.json"
  expect_identical(version(package), "2.2-rc.1")
  package$`$schema` <- "https://datapackage.org/profiles/3.0/datapackage.json"
  expect_identical(version(package), "3.0")

  # Custom extensions
  package$`$schema` <- "https://custom.datapackage.org/package-profile.json"
  expect_identical(version(package), ">=2.0")
  package$`$schema` <- "https://rs.tdwg.org/dwc-dp/1.0/dwc-dp-profile.json"
  expect_identical(version(package), ">=2.0")
})

## Resource ----
test_that("version() returns correct version for resource", {
  resource <- list(
    name = "custom_resource",
    path = "https://example.com/data.csv"
  )

  # Undefined $schema
  resource$`$schema` <- NULL
  expect_identical(version(resource), "1.0")

  # Defined default
  resource$`$schema` <- "https://datapackage.org/profiles/1.0/dataresource.json"
  expect_identical(version(resource), "1.0")

  # 2.0
  resource$`$schema` <- "https://datapackage.org/profiles/2.0/dataresource.json"
  expect_identical(version(resource), "2.0")

  # Future versions
  resource$`$schema` <- "https://datapackage.org/profiles/2.1/dataresource.json"
  expect_identical(version(resource), "2.1")
  resource$`$schema` <- "https://datapackage.org/profiles/2.2-rc.1/dataresource.json"
  expect_identical(version(resource), "2.2-rc.1")
  resource$`$schema` <- "https://datapackage.org/profiles/3.0/dataresource.json"
  expect_identical(version(resource), "3.0")

  # Custom extensions
  resource$`$schema` <- "https://custom.datapackage.org/resource-profile.json"
  expect_identical(version(resource), ">=2.0")
})

## Dialect ----
test_that("version() returns correct version for dialect", {
  # Entire dialect undefined => $schema is undefined => 1.0
  dialect <- NULL
  expect_identical(version(dialect), "1.0")

  # Undefined $schema
  dialect <- list(
    delimiter = ","
  )
  dialect$`$schema` <- NULL
  expect_identical(version(dialect), "1.0")

  # Defined default
  dialect$`$schema` <- "https://datapackage.org/profiles/1.0/tabledialect.json"
  expect_identical(version(dialect), "1.0")

  # 2.0
  dialect$`$schema` <- "https://datapackage.org/profiles/2.0/tabledialect.json"
  expect_identical(version(dialect), "2.0")

  # Future versions
  dialect$`$schema` <- "https://datapackage.org/profiles/2.1/tabledialect.json"
  expect_identical(version(dialect), "2.1")
  dialect$`$schema` <- "https://datapackage.org/profiles/2.2-rc.1/tabledialect.json"
  expect_identical(version(dialect), "2.2-rc.1")
  dialect$`$schema` <- "https://datapackage.org/profiles/3.0/tabledialect.json"
  expect_identical(version(dialect), "3.0")

  # Custom extensions
  dialect$`$schema` <- "https://custom.datapackage.org/dialect-profile.json"
  expect_identical(version(dialect), ">=2.0")
})

## Schema ----
test_that("version() returns correct version for schema", {
  schema <- list(
    fields = list()
  )

  # Undefined $schema
  schema$`$schema` <- NULL
  expect_identical(version(schema), "1.0")

  # Defined default
  schema$`$schema` <- "https://datapackage.org/profiles/1.0/tableschema.json"
  expect_identical(version(schema), "1.0")

  # 2.0
  schema$`$schema` <- "https://datapackage.org/profiles/2.0/tableschema.json"
  expect_identical(version(schema), "2.0")

  # Future versions
  schema$`$schema` <- "https://datapackage.org/profiles/2.1/tableschema.json"
  expect_identical(version(schema), "2.1")
  schema$`$schema` <- "https://datapackage.org/profiles/2.2-rc.1/tableschema.json"
  expect_identical(version(schema), "2.2-rc.1")
  schema$`$schema` <- "https://datapackage.org/profiles/3.0/tableschema.json"
  expect_identical(version(schema), "3.0")

  # Custom extensions
  schema$`$schema` <- "https://custom.datapackage.org/schema-profile.json"
  expect_identical(version(schema), ">=2.0")
})

## Example package ----
test_that("version() returns correct version for example package properties", {
  p_v1 <- example_package(version = "1.0")

  # Data Package
  expect_identical(version(p_v1), "1.0")
  # Data Resource
  expect_identical(version(resource(p_v1, "deployments")), "1.0")
  # Data Dialect (undefined for deployments)
  expect_identical(version(resource(p_v1, "deployments")$dialect), "1.0")
  # Table Dialect (defined for observations)
  expect_identical(version(resource(p_v1, "observations")$dialect), "1.0")
  # Table Schema
  expect_identical(version(resource(p_v1, "deployments")$schema), "1.0")

  p_v2 <- example_package(version = "2.0")

  # Data Package
  expect_identical(version(p_v2), "2.0")
  # Data Resource
  expect_identical(version(resource(p_v2, "deployments")), "2.0")
  # Data Dialect (undefined for deployments)
  expect_identical(version(resource(p_v2, "deployments")$dialect), "1.0")
  # Table Dialect (defined for observations)
  expect_identical(version(resource(p_v2, "observations")$dialect), "2.0")
  # Table Schema
  expect_identical(version(resource(p_v2, "deployments")$schema), "2.0")
})

## Invalid ----
test_that("version() returns >=2.0 for invalid $schema", {
  x <- list()
  x$`$schema` <- list()
  expect_identical(version(x), ">=2.0")
  x$`$schema` <- 3.0
  expect_identical(version(x), ">=2.0")
})
