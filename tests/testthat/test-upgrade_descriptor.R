test_that("upgrade_descriptor() upgrades a v1 package to v2", {
  p_v1 <- example_package(version = "1.0")
  expect_identical(version(upgrade_descriptor(p_v1)), "2.0")
})

test_that("upgrade_descriptor() leaves a v2 package as is", {
  p_v2 <- example_package(version = "2.0")
  expect_identical(upgrade_descriptor(p_v2), p_v2)
})

test_that("upgrade_descriptor() removes profile", {
  p_v1 <- example_package(version = "1.0")
  p_v1$profile <- "data-package"
  expect_null(upgrade_descriptor(p_v1)$profile)
})

test_that("upgrade_descriptor() sets $schema as first property", {
  p_v1 <- example_package(version = "1.0")
  v2_profile <- "https://datapackage.org/profiles/2.0/datapackage.json"

  # Ignore undefined
  p_v1$profile <- NULL
  expect_identical(upgrade_descriptor(p_v1)[[1]], v2_profile)

  # Ignore default value
  p_v1$profile <- "data-package"
  expect_identical(upgrade_descriptor(p_v1)[[1]], v2_profile)

  # Ignore tabular-data-package (deprecated)
  p_v1$profile <- "tabular-data-package"
  expect_identical(upgrade_descriptor(p_v1)[[1]], v2_profile)

  # Keep fiscal-data-package (but set to URL)
  p_v1$profile <- "fiscal-data-package"
  expect_identical(
    upgrade_descriptor(p_v1)[[1]],
    "https://specs.frictionlessdata.io/schemas/fiscal-data-package.json"
  )

  # Keep URL to custom profile
  custom_profile_url <- "https://rs.tdwg.org/dwc-dp/1.0/dwc-dp-profile.json"
  p_v1$profile <- custom_profile_url
  expect_identical(upgrade_descriptor(p_v1)[[1]], custom_profile_url)

  # Ignore unregistered value
  p_v1$profile <- "unregistered-package"
  expect_identical(upgrade_descriptor(p_v1)[[1]], v2_profile)
})
