library(testthat)

if (!exists("tyler_max_table")) {
  source(testthat::test_path("..", "..", "R", "MaxTable.R"))
}
# Assuming your package is loaded which contains the tyler_max_table function

# Define necessary vectors
vec <- factor(c("A", "B", "C", "B", "B"))  # B appears most frequently
empty_vec <- factor(character(0))          # Empty factor

# Tests for tyler_max_table
test_that("tyler_max_table converts non-factor to factor and computes correctly", {
  expect_equal(tyler_max_table(vec), "B")
  expect_equal(tyler_max_table(vec, mult = TRUE), c("B"))
})

test_that("tyler_max_table returns empty when input is empty", {
  expect_equal(tyler_max_table(empty_vec), character(0))
  expect_equal(tyler_max_table(empty_vec, mult = TRUE), character(0))
})

test_that("tyler_max_table handles basic input correctly", {
  vec <- factor(c("A", "B", "A", "C", "B", "B"))
  expect_equal(tyler_max_table(vec), "B")
  expect_equal(tyler_max_table(vec, mult = TRUE), c("B"))
})

test_that("tyler_max_table converts non-factor to factor and computes correctly", {
  vec <- c("A", "B", "A", "C", "B", "B")  # Not initially a factor
  expect_equal(tyler_max_table(vec), "B")
  expect_equal(tyler_max_table(vec, mult = TRUE), c("B"))
})

test_that("tyler_max_table returns multiple maximums when tied and mult=TRUE", {
  vec <- factor(c("A", "B", "A", "C", "B", "C"))
  expect_equal(tyler_max_table(vec, mult = TRUE), c("A", "B", "C"))
})

test_that("tyler_max_table handles single element vector", {
  vec <- factor("A")
  expect_equal(tyler_max_table(vec), "A")
  expect_equal(tyler_max_table(vec, mult = TRUE), c("A"))
})

test_that("tyler_max_table handles vector with all elements same", {
  vec <- factor(c("B", "B", "B"))
  expect_equal(tyler_max_table(vec), "B")
  expect_equal(tyler_max_table(vec, mult = TRUE), c("B"))
})

# tyler_max_table Tests
test_that("tyler_max_table converts non-factor to factor and computes correctly", {
  expect_equal(tyler_max_table(vec), "B")
  expect_equal(tyler_max_table(vec, mult = TRUE), c("B"))
})

test_that("tyler_max_table returns empty when input is empty", {
  expect_equal(tyler_max_table(empty_vec), character(0))
  expect_equal(tyler_max_table(empty_vec, mult = TRUE), character(0))
})
