# unique_sorted() ----
test_that("unique_sorted() returns unique values sorted by descending count", {
  x <- c("a", "b", "b", "b", "c", "a")
  expect_identical(unique_sorted(x), c("b", "a", "c"))

  x_na <- c(NA_character_, NA_character_)
  expect_identical(unique_sorted(x_na), character(0))

  x_part_na <- c(NA_character_, NA_character_, "a")
  expect_identical(unique_sorted(x_part_na), "a")
})

# clean_list() ----
test_that("clean_list() removes elements from list that match condition", {
  x <- list(
    a = list(list(x = "value", y = NULL, z = list(v = NULL))),
    b = NULL
  )

  # Default (non recursive remove NULL): b removed
  expect_identical(
    clean_list(x),
    list(
      a = list(list(x = "value", y = NULL, z = list(v = NULL)))
    )
  )

  # Remove recursive: a[[1]]$y removed
  expected_list <- list(
    a = list(list(x = "value", z = list()))
  )
  names(expected_list$a[[1]]$z) <- character(0) # named list()
  expect_identical(
    clean_list(x, recursive = TRUE),
    expected_list
  )

  # Remove recursive + empty elements: a[[1]]$z removed
  expect_identical(
    clean_list(
      x,
      function(x) is.null(x) | length(x) == 0L,
      recursive = TRUE
    ),
    list(
      a = list(list(x = "value"))
    )
  )
})

# character_to_list() ----
test_that("character_to_list() converts character to list", {
  expect_identical(character_to_list("a"), list("a")) # Update
  expect_identical(character_to_list(c("a", "b")), list("a", "b")) # Update
  expect_identical(character_to_list(list("a", "b")), list("a", "b")) # Keep
  expect_identical(character_to_list(1), 1) # Keep
  expect_null(character_to_list(NULL)) # Keep
})

# is_url() ----
test_that("is_url() tests whether path is URL", {
  expect_true(is_url("http://example.com"))
  expect_true(is_url("https://example.com"))
  expect_true(is_url("ftp://example.com"))
  expect_true(is_url("sftp://example.com"))

  expect_false(is_url("path/to/file"))
  expect_false(is_url("/path/to/file"))
  expect_false(is_url("../path/to/file"))
  expect_false(is_url("http:/example.com"))
  expect_true(is_url("ftps://example.com")) # Not a correct protocol
})

# get_dot_names() ----
test_that("get_dot_names() returns the names passed via ...", {
  test_fn <- function(...) {
    get_dot_names(...)
  }
  expect_identical(test_fn(a = "1", b = "2"), c("a", "b"))
})

test_that("get_dot_names() does not return empty strings for unnamed args passed
           to ellipsis", {
  test_fn <- function(...) {
    get_dot_names(...)
  }
  expect_identical(test_fn(a = "1", "2", b = "3", c = "4"), c("a", "b", "c"))
  expect_length(test_fn(a = "1", "2", b = "3", c = "4"), 3)
})

# append_with_attributes() ----
test_that("append_with_attributes() supports base::append() functionality", {
  list_1 <- list(a = 1, b = 2)
  list_2 <- list(c = 3, d = 4)
  list_nested <- list(c = list(d = 3))
  expect_identical(
    append_with_attributes(1:5, 0:1, after = 3),
    append(1:5, 0:1, after = 3)
  )
  expect_identical(
    append_with_attributes(list_1, list_2, after = 0),
    append(list_1, list_2, after = 0)
  )
  expect_identical(
    append_with_attributes(list_1, list_nested),
    append(list_1, list_nested)
  )
})

test_that("append_with_attributes() preserves attributes and classes", {
  object <- head(letters, -1L) # a, b, ... y
  # Add custom attributes
  attr(object, "title") <- "Letters"
  attr(object, "description") <- "Lowercase letters in alphabetical order"
  # Add custom class
  class(object) <- c("letters")
  class(object) <- c("series", class(object))

  expect_identical(
    attributes(append_with_attributes(object, "z")),
    attributes(object)
  )
  expect_s3_class(
    append_with_attributes(object, "z"),
    c("series", "letters")
  )
})

test_that("append_with_attributes() preserves names", {
  object <- head(letters, -1L) # a, b, ... y
  # Add names
  object <- purrr::set_names(object, toupper(object))
  expect_named(
    append_with_attributes(object, c("Z" = "z")),
    toupper(letters)
  )
})

test_that("append_with_attributes() can return a valid package", {
  p <- create_package()
  p_appended <- append_with_attributes(
    p,
    list(custom_property = "custom_value"),
    0
  )
  expect_no_error(check_package(p_appended))
  expect_identical(p_appended[[1]], "custom_value")
})

test_that("append_with_attributes() can return a valid resource", {
  resource <- resource(example_package(), "deployments")
  resource_appended <- append_with_attributes(
    resource,
    list(first = "custom_value"),
    0
  )
  # Resource attributes are kept
  expect_identical(
    attr(resource_appended, "data_location"),
    attr(resource, "data_location")
  )
  expect_identical(resource_appended[[1]], "custom_value")
})


