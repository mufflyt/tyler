library(testthat)

# Assuming definition of vec for testing multiple minimums
vec <- factor(c("A", "B", "C", "C", "B", "A"))


test_that("MinTable returns empty with empty input", {
  empty_vec <- factor(character(0))
  expect_equal(MinTable(empty_vec), character(0))
  expect_equal(MinTable(empty_vec, mult = TRUE), character(0))
})

test_that("MaxTable returns empty when input is empty", {
  empty_vec <- factor(character(0))
  expect_equal(MaxTable(empty_vec, mult = TRUE), character(0))
})


