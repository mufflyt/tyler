library(testthat)

if (!exists("tyler_min_table")) {
  source(testthat::test_path("..", "..", "R", "MinTable.R"))
}

if (!exists("tyler_max_table")) {
  source(testthat::test_path("..", "..", "R", "MaxTable.R"))
}

# Assuming definition of vec for testing multiple minimums
vec <- factor(c("A", "B", "C", "C", "B", "A"))


test_that("tyler_min_table returns empty with empty input", {
  empty_vec <- factor(character(0))
  expect_equal(tyler_min_table(empty_vec), character(0))
  expect_equal(tyler_min_table(empty_vec, mult = TRUE), character(0))
})

test_that("tyler_max_table returns empty when input is empty", {
  empty_vec <- factor(character(0))
  expect_equal(tyler_max_table(empty_vec, mult = TRUE), character(0))
})


