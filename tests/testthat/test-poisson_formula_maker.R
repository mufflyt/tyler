library(testthat)
library(dplyr)

# Sample data for testing
sample_data <- data.frame(
  days = c(5, 10, 15),
  age = c(30, 40, 50),
  name = c("A", "B", "C"),
  stringsAsFactors = FALSE
)

# Test cases
test_that("Creates formula without random effect", {
  cat("Running test: Creates formula without random effect\n")
  result <- create_formula(sample_data, "days")

  expected_formula <- as.formula("days ~ `age` + `name`")

  expect_equal(as.character(result), as.character(expected_formula))
})

test_that("Creates formula with random effect", {
  cat("Running test: Creates formula with random effect\n")
  result <- create_formula(sample_data, "days", "name")

  expected_formula <- as.formula("days ~ `age` + `name` + (1 | name)")

  expect_equal(as.character(result), as.character(expected_formula))
})
