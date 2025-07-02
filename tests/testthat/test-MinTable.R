library(testthat)

# Assuming definition of vec for testing multiple minimums
vec <- factor(c("A", "B", "C", "C", "B", "A"))


test_that("min_table returns empty with empty input", {
  empty_vec <- factor(character(0))
  expect_equal(min_table(empty_vec), character(0))
  expect_equal(min_table(empty_vec, mult = TRUE), character(0))
})

test_that("max_table returns empty when input is empty", {
  empty_vec <- factor(character(0))
  expect_equal(max_table(empty_vec, mult = TRUE), character(0))
})


