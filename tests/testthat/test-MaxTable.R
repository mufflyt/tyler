library(testthat)
# Assuming your package is loaded which contains the MaxTable function

# Define necessary vectors
vec <- factor(c("A", "B", "C", "B", "B"))  # B appears most frequently
empty_vec <- factor(character(0))          # Empty factor

# Tests for MaxTable
test_that("MaxTable converts non-factor to factor and computes correctly", {
  expect_equal(MaxTable(vec), "B")
  expect_equal(MaxTable(vec, mult = TRUE), c("B"))
})

test_that("MaxTable returns empty when input is empty", {
  expect_equal(MaxTable(empty_vec), character(0))
  expect_equal(MaxTable(empty_vec, mult = TRUE), character(0))
})

test_that("MaxTable handles basic input correctly", {
  vec <- factor(c("A", "B", "A", "C", "B", "B"))
  expect_equal(MaxTable(vec), "B")
  expect_equal(MaxTable(vec, mult = TRUE), c("B"))
})

test_that("MaxTable converts non-factor to factor and computes correctly", {
  vec <- c("A", "B", "A", "C", "B", "B")  # Not initially a factor
  expect_equal(MaxTable(vec), "B")
  expect_equal(MaxTable(vec, mult = TRUE), c("B"))
})

test_that("MaxTable returns multiple maximums when tied and mult=TRUE", {
  vec <- factor(c("A", "B", "A", "C", "B", "C"))
  expect_equal(MaxTable(vec, mult = TRUE), c("A", "B", "C"))
})

test_that("MaxTable handles single element vector", {
  vec <- factor("A")
  expect_equal(MaxTable(vec), "A")
  expect_equal(MaxTable(vec, mult = TRUE), c("A"))
})

test_that("MaxTable handles vector with all elements same", {
  vec <- factor(c("B", "B", "B"))
  expect_equal(MaxTable(vec), "B")
  expect_equal(MaxTable(vec, mult = TRUE), c("B"))
})

# MaxTable Tests
test_that("MaxTable converts non-factor to factor and computes correctly", {
  expect_equal(MaxTable(vec), "B")
  expect_equal(MaxTable(vec, mult = TRUE), c("B"))
})

test_that("MaxTable returns empty when input is empty", {
  expect_equal(MaxTable(empty_vec), character(0))
  expect_equal(MaxTable(empty_vec, mult = TRUE), character(0))
})
