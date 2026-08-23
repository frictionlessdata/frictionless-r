test_that("version() returns correct version for Data Package", {
  package <- create_package()

  # Undefined $schema
  package$`$schema` <- NULL
  expect_equal(version(package), "1.0")

  # Defined default
  package$`$schema` <- "https://datapackage.org/profiles/1.0/datapackage.json"
  expect_equal(version(package), "1.0")

  # 2.0
  package$`$schema` <- "https://datapackage.org/profiles/2.0/datapackage.json"
  expect_equal(version(package), "2.0")

  # Future versions
  package$`$schema` <- "https://datapackage.org/profiles/2.1/datapackage.json"
  expect_equal(version(package), "2.1")
  package$`$schema` <- "https://datapackage.org/profiles/2.2-rc.1/datapackage.json"
  expect_equal(version(package), "2.2-rc.1")
  package$`$schema` <- "https://datapackage.org/profiles/3.0/datapackage.json"
  expect_equal(version(package), "3.0")

  # Custom extensions
  package$`$schema` <- "https://custom.datapackage.org/package-profile.json"
  expect_equal(version(package), ">=2.0")
  package$`$schema` <- "https://rs.tdwg.org/dwc-dp/1.0/dwc-dp-profile.json"
  expect_equal(version(package), ">=2.0")
})

test_that("version() returns correct version for Data Resource", {
  resource <- list(
    name = "custom_resource",
    path = "https://example.com/data.csv"
  )

  # Undefined $schema
  resource$`$schema` <- NULL
  expect_equal(version(resource), "1.0")

  # Defined default
  resource$`$schema` <- "https://datapackage.org/profiles/1.0/dataresource.json"
  expect_equal(version(resource), "1.0")

  # 2.0
  resource$`$schema` <- "https://datapackage.org/profiles/2.0/dataresource.json"
  expect_equal(version(resource), "2.0")

  # Future versions
  resource$`$schema` <- "https://datapackage.org/profiles/2.1/dataresource.json"
  expect_equal(version(resource), "2.1")
  resource$`$schema` <- "https://datapackage.org/profiles/2.2-rc.1/dataresource.json"
  expect_equal(version(resource), "2.2-rc.1")
  resource$`$schema` <- "https://datapackage.org/profiles/3.0/dataresource.json"
  expect_equal(version(resource), "3.0")

  # Custom extensions
  resource$`$schema` <- "https://custom.datapackage.org/resource-profile.json"
  expect_equal(version(resource), ">=2.0")
})

test_that("version() returns correct version for Table Dialect", {
  # Entire dialect undefined => $schema is undefined => 1.0
  dialect <- NULL
  expect_equal(version(dialect), "1.0")

  # Undefined $schema
  dialect <- list(
    delimiter = ","
  )
  dialect$`$schema` <- NULL
  expect_equal(version(dialect), "1.0")

  # Defined default
  dialect$`$schema` <- "https://datapackage.org/profiles/1.0/tabledialect.json"
  expect_equal(version(dialect), "1.0")

  # 2.0
  dialect$`$schema` <- "https://datapackage.org/profiles/2.0/tabledialect.json"
  expect_equal(version(dialect), "2.0")

  # Future versions
  dialect$`$schema` <- "https://datapackage.org/profiles/2.1/tabledialect.json"
  expect_equal(version(dialect), "2.1")
  dialect$`$schema` <- "https://datapackage.org/profiles/2.2-rc.1/tabledialect.json"
  expect_equal(version(dialect), "2.2-rc.1")
  dialect$`$schema` <- "https://datapackage.org/profiles/3.0/tabledialect.json"
  expect_equal(version(dialect), "3.0")

  # Custom extensions
  dialect$`$schema` <- "https://custom.datapackage.org/dialect-profile.json"
  expect_equal(version(dialect), ">=2.0")
})

test_that("version() returns correct version for Table Schema", {
  schema <- list(
    fields = list()
  )

  # Undefined $schema
  schema$`$schema` <- NULL
  expect_equal(version(schema), "1.0")

  # Defined default
  schema$`$schema` <- "https://datapackage.org/profiles/1.0/tableschema.json"
  expect_equal(version(schema), "1.0")

  # 2.0
  schema$`$schema` <- "https://datapackage.org/profiles/2.0/tableschema.json"
  expect_equal(version(schema), "2.0")

  # Future versions
  schema$`$schema` <- "https://datapackage.org/profiles/2.1/tableschema.json"
  expect_equal(version(schema), "2.1")
  schema$`$schema` <- "https://datapackage.org/profiles/2.2-rc.1/tableschema.json"
  expect_equal(version(schema), "2.2-rc.1")
  schema$`$schema` <- "https://datapackage.org/profiles/3.0/tableschema.json"
  expect_equal(version(schema), "3.0")

  # Custom extensions
  schema$`$schema` <- "https://custom.datapackage.org/schema-profile.json"
  expect_equal(version(schema), ">=2.0")
})

test_that("version() returns >=2.0 for invalid $schema", {
  x <- list()
  x$`$schema` <- list()
  expect_equal(version(x), ">=2.0")
  x$`$schema` <- 3.0
  expect_equal(version(x), ">=2.0")
})

test_that("version() returns correct version for example package properties", {
  p_1.0 <- example_package(version = "1.0")

  # Data Package
  expect_equal(version(p_1.0), "1.0")
  # Data Resource
  expect_equal(version(p_1.0$resources[[1]]), "1.0")
  # Data Dialect (undefined for deployments)
  expect_equal(version(p_1.0$resources[[1]]$dialect), "1.0")
  # Table Dialect (defined for observations)
  expect_equal(version(p_1.0$resources[[2]]$dialect), "1.0")
  # Table Schema
  expect_equal(version(p_1.0$resources[[1]]$schema), "1.0")

  p_2.0 <- example_package(version = "2.0")

  # Data Package
  expect_equal(version(p_2.0), "2.0")
  # Data Resource
  expect_equal(version(p_2.0$resources[[1]]), "2.0")
  # Data Dialect (undefined for deployments)
  expect_equal(version(p_2.0$resources[[1]]$dialect), "1.0")
  # Table Dialect (defined for observations)
  expect_equal(version(p_2.0$resources[[2]]$dialect), "2.0")
  # Table Schema
  expect_equal(version(p_2.0$resources[[1]]$schema), "2.0")
})
