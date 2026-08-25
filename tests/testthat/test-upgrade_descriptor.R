test_that("upgrade_descriptor() returns a valid package", {
  p_v1 <- example_package(version = "1.0")
  expect_no_error(check_package(upgrade_descriptor(p_v1)))
})

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
  expect_identical(upgrade_descriptor(p_v1)$`$schema`, v2_profile)
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

test_that("upgrade_descriptor() updates contributor role to roles", {
  p_v1 <- example_package(version = "1.0")

  # Defined
  p_v1$contributors <- list(
    list(title = "First author", role = "author"),
    list(title = "No role", custom_property = "custom"),
    list(title = "Jack of all trades", roles = list("a", "b"))
  )
  p_upgraded <- upgrade_descriptor(p_v1)
  expect_identical(p_upgraded$contributors[[1]]$roles, list("author")) # Added
  expect_null(p_upgraded$contributors[[1]]$role) # Removed
  expect_null(p_upgraded$contributors[[2]]$roles) # Not added
  expect_identical(p_upgraded$contributors[[3]]$roles, list("a", "b")) # Kept

  # Undefined
  p_v1$contributors <- NULL
  expect_null(upgrade_descriptor(p_v1)$contributors)
})
