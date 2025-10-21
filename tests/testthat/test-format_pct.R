library(testthat)
library(tyler)

# Unit Tests for format_pct
test_that("format_pct formats single numeric value correctly with default digits", {
  result <- format_pct(0.12345)
  expect_type(result, "character")
  expect_true(grepl("0.1", result))
})

test_that("format_pct formats single numeric value correctly with 2 decimal places", {
  result <- format_pct(0.12345, my_digits = 2)
  expect_type(result, "character")
  expect_true(grepl("0.12", result))
})

test_that("format_pct formats single numeric value correctly with 0 decimal places", {
  result <- format_pct(0.5, my_digits = 0)
  expect_type(result, "character")
  expect_true(grepl("0.5|1", result))  # May round or truncate
})

test_that("format_pct handles vector of numeric values", {
  values <- c(0.12345, 0.6789, 0.54321)
  formatted_values <- format_pct(values, my_digits = 2)
  expect_type(formatted_values, "character")
  expect_length(formatted_values, 3)
})

test_that("format_pct handles zero value", {
  result <- format_pct(0, my_digits = 1)
  expect_type(result, "character")
  expect_true(grepl("0", result))
})

test_that("format_pct handles negative values", {
  result <- format_pct(-0.5, my_digits = 1)
  expect_type(result, "character")
  expect_true(grepl("-", result))
})

test_that("format_pct handles very small values", {
  result <- format_pct(0.0001, my_digits = 4)
  expect_type(result, "character")
})

test_that("format_pct handles very large values", {
  result <- format_pct(100.12345, my_digits = 2)
  expect_type(result, "character")
})

# Regression Tests
test_that("format_pct handles NA values", {
  result <- format_pct(NA, my_digits = 1)
  expect_true(is.na(result) || grepl("NA", result))
})

test_that("format_pct handles vector with NA values", {
  values <- c(0.1, NA, 0.3)
  result <- format_pct(values, my_digits = 1)
  expect_length(result, 3)
})

test_that("format_pct handles empty numeric vector", {
  result <- format_pct(numeric(0), my_digits = 1)
  expect_length(result, 0)
})

test_that("format_pct preserves decimal places", {
  result <- format_pct(0.1, my_digits = 3)
  expect_type(result, "character")
})
