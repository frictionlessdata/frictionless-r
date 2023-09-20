.add_field_basic_metadata <- function(name, type, description) {
  list(
      "name" = name,
      "type" = type,
      "description" = description
    )
}

# Create a Data Package and add the "iris" data frame as a resource
my_package <- create_package() %>%
  add_resource(resource_name = "iris", data = iris)


iris_schema <- my_package %>%
  get_schema("iris")

test_that("add_description(): Iris example", {
  descriptions <- c(
    "Sepal length in cm.",
    "Sepal width in cm.",
    "Pedal length in cm.",
    "Pedal width in cm.",
    "Iris species."
  )
  expected <- list("fields" = list(
    .add_field_basic_metadata("Sepal.Length", "number", descriptions[1]),
    .add_field_basic_metadata("Sepal.Width", "number", descriptions[2]),
    .add_field_basic_metadata("Petal.Length", "number", descriptions[3]),
    .add_field_basic_metadata("Petal.Width", "number", descriptions[4]),
    list(
      "name" = "Species",
      "type" = "string",
      "constraints" = list("enum" = c("setosa", "versicolor", "virginica")),
      "description" = descriptions[5]
    )
  ))
  obtained <- iris_schema |>
    add_description(descriptions)
  expect_equal(obtained, expected)
})

