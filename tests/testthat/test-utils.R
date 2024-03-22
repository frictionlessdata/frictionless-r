test_that("replace_null() replaces NULL or returns value", {
  expect_identical(replace_null(NULL, "y"), "y")
  expect_identical(replace_null(NA, "y"), NA)
  expect_identical(replace_null(1, "y"), 1)
  expect_identical(replace_null("x", "y"), "x")
})

test_that("unique_sorted() returns unique values sorted by descending count", {
  x <- c("a", "b", "b", "b", "c", "a")
  expect_identical(unique_sorted(x), c("b", "a", "c"))

  x_na <- c(NA_character_, NA_character_)
  expect_identical(unique_sorted(x_na), character(0))

  x_part_na <- c(NA_character_, NA_character_, "a")
  expect_identical(unique_sorted(x_part_na), "a")
})

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
