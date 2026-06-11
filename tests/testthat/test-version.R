test_that("version() returns version based on $schema", {
  p <- create_package()

  # Undefined
  p$`$schema` <- NULL
  expect_equal(version(p), "1.0")

  # Defined default
  p$`$schema` <- "https://datapackage.org/profiles/1.0/datapackage.json"
  expect_equal(version(p), "1.0")

  # 2.0
  p$`$schema` <- "https://datapackage.org/profiles/2.0/datapackage.json"
  expect_equal(version(p), "2.0")

  # Future versions
  p$`$schema` <- "https://datapackage.org/profiles/2.1/datapackage.json"
  expect_equal(version(p), "2.1")
  p$`$schema` <- "https://datapackage.org/profiles/2.2-rc.1/datapackage.json"
  expect_equal(version(p), "2.2-rc.1")
  p$`$schema` <- "https://datapackage.org/profiles/3.0/datapackage.json"
  expect_equal(version(p), "3.0")

  # Custom extensions
  p$`$schema` <- "https://spatial.datapackage.org/profiles/1.0/datapackage.json"
  expect_equal(version(p), "2.x")
  p$`$schema` <- "http://rs.tdwg.org/dwc-dp/1.0/dwc-dp-profile.json"
  expect_equal(version(p), "2.x")
})

test_that("version() returns 2.x for invalid $schema", {
  p <- create_package()
  p$`$schema` <- list()
  expect_equal(version(p), "2.x")
  p$`$schema` <- 3.0
  expect_equal(version(p), "2.x")
})

test_that("version() returns correct version for example packages", {
  p_1.0 <- example_package(version = "1.0")
  expect_equal(version(p_1.0), "1.0")
  p_2.0 <- example_package(version = "2.0")
  expect_equal(version(p_2.0), "2.0")
})
