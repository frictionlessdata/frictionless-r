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
    list(
      "name" = "Sepal.Length",
      "type" = "number",
      "description" = descriptions[1]
    ),
    list(
      "name" = "Sepal.Width",
      "type" = "number",
      "description" = descriptions[2]
    ),
    list(
      "name" = "Petal.Length",
      "type" = "number",
      "description" = descriptions[3]
    ),
    list(
      "name" = "Petal.Width",
      "type" = "number",
      "description" = descriptions[4]
    ),
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
