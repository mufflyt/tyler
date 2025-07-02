library(testthat)
# Assuming your package is loaded which contains the max_table function

# Define necessary vectors
vec <- factor(c("A", "B", "C", "B", "B"))  # B appears most frequently
empty_vec <- factor(character(0))          # Empty factor

# Tests for max_table
test_that("max_table converts non-factor to factor and computes correctly", {
  expect_equal(max_table(vec), "B")
  expect_equal(max_table(vec, mult = TRUE), c("B"))
})

test_that("max_table returns empty when input is empty", {
  expect_equal(max_table(empty_vec), character(0))
  expect_equal(max_table(empty_vec, mult = TRUE), character(0))
})

test_that("max_table handles basic input correctly", {
  vec <- factor(c("A", "B", "A", "C", "B", "B"))
  expect_equal(max_table(vec), "B")
  expect_equal(max_table(vec, mult = TRUE), c("B"))
})

test_that("max_table converts non-factor to factor and computes correctly", {
  vec <- c("A", "B", "A", "C", "B", "B")  # Not initially a factor
  expect_equal(max_table(vec), "B")
  expect_equal(max_table(vec, mult = TRUE), c("B"))
})

test_that("max_table returns multiple maximums when tied and mult=TRUE", {
  vec <- factor(c("A", "B", "A", "C", "B", "C"))
  expect_equal(max_table(vec, mult = TRUE), c("A", "B", "C"))
})

test_that("max_table handles single element vector", {
  vec <- factor("A")
  expect_equal(max_table(vec), "A")
  expect_equal(max_table(vec, mult = TRUE), c("A"))
})

test_that("max_table handles vector with all elements same", {
  vec <- factor(c("B", "B", "B"))
  expect_equal(max_table(vec), "B")
  expect_equal(max_table(vec, mult = TRUE), c("B"))
})

# max_table Tests
test_that("max_table converts non-factor to factor and computes correctly", {
  expect_equal(max_table(vec), "B")
  expect_equal(max_table(vec, mult = TRUE), c("B"))
})

test_that("max_table returns empty when input is empty", {
  expect_equal(max_table(empty_vec), character(0))
  expect_equal(max_table(empty_vec, mult = TRUE), character(0))
})
