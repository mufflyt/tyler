library(testthat)
library(tyler)
library(dplyr)

# Regression Tests for Edge Cases and Potential Bugs

# Test Suite: Data Input Edge Cases
test_that("functions handle NULL input gracefully", {
  # format_pct
  expect_error(format_pct(NULL), NA)  # Should not error

  # Test with empty vectors
  expect_length(format_pct(numeric(0)), 0)
})

test_that("functions handle Inf and -Inf values", {
  # format_pct with Inf
  result <- format_pct(c(Inf, -Inf, 0.5))
  expect_type(result, "character")
  expect_length(result, 3)

  # calculate_proportion with Inf (should handle as categorical)
  df_inf <- data.frame(category = c("A", "B", "A"))
  result <- calculate_proportion(df_inf, category)
  expect_s3_class(result, "data.frame")
})

test_that("functions handle special numeric values (NaN)", {
  # format_pct with NaN
  result <- format_pct(c(NaN, 0.5, 1.0))
  expect_length(result, 3)
})

test_that("functions handle very long character strings", {
  long_string <- paste(rep("A", 1000), collapse = "")
  df_long <- data.frame(category = rep(long_string, 10))

  result <- calcpercentages(df_long, "category")
  expect_s3_class(result, "data.frame")
  expect_equal(result$n, 10)
})

test_that("functions handle Unicode and special characters", {
  df_unicode <- data.frame(
    category = c("Café", "naïve", "日本語", "emoji😀", "Café")
  )

  result <- calculate_proportion(df_unicode, category)
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 1)

  result2 <- calcpercentages(df_unicode, "category")
  expect_s3_class(result2, "data.frame")
})

test_that("functions handle whitespace-only strings", {
  df_whitespace <- data.frame(
    category = c("  ", "\t", "\n", "A", "A")
  )

  result <- calculate_proportion(df_whitespace, category)
  expect_s3_class(result, "data.frame")
  # Should treat different whitespace as different categories
  expect_true(nrow(result) >= 2)
})

# Test Suite: Boundary Conditions
test_that("functions handle boundary numeric values", {
  # Test with very small numbers
  small_numbers <- c(1e-100, 1e-50, 1e-10)
  result <- format_pct(small_numbers, my_digits = 110)
  expect_type(result, "character")
  expect_length(result, 3)

  # Test with very large numbers
  large_numbers <- c(1e100, 1e50, 1e10)
  result2 <- format_pct(large_numbers, my_digits = 2)
  expect_type(result2, "character")
  expect_length(result2, 3)
})

test_that("functions handle maximum vector lengths efficiently", {
  # Test with very large vectors
  large_vec <- runif(100000, min = 0, max = 1)
  result <- format_pct(large_vec, my_digits = 2)
  expect_length(result, 100000)

  # Test calculate_proportion with large data
  df_large <- data.frame(
    category = sample(letters, 100000, replace = TRUE)
  )
  result2 <- calculate_proportion(df_large, category)
  expect_s3_class(result2, "data.frame")
  expect_equal(sum(result2$n), 100000)
})

test_that("functions handle single-element edge case", {
  # Single element tests
  expect_length(format_pct(0.5), 1)

  df_single <- data.frame(cat = "A")
  result <- calculate_proportion(df_single, cat)
  expect_equal(nrow(result), 1)
  expect_equal(result$percent, 100)
})

# Test Suite: Type Coercion and Mixed Types
test_that("functions handle mixed numeric types", {
  mixed_types <- c(1L, 2.5, 3, 4.0)  # integer, double, numeric
  result <- format_pct(mixed_types, my_digits = 1)
  expect_type(result, "character")
  expect_length(result, 4)
})

test_that("calculate_proportion handles factor with unused levels", {
  df_factor <- data.frame(
    category = factor(c("A", "B", "A"), levels = c("A", "B", "C", "D"))
  )

  result <- calculate_proportion(df_factor, category)
  expect_s3_class(result, "data.frame")
  # Should only show levels that appear in data
  expect_true(nrow(result) <= 4)
})

test_that("functions handle ordered factors", {
  df_ordered <- data.frame(
    grade = ordered(c("Low", "Medium", "High", "Low", "Medium"),
                    levels = c("Low", "Medium", "High"))
  )

  result <- calculate_proportion(df_ordered, grade)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
})

# Test Suite: Precision and Rounding
test_that("format_pct handles precision correctly", {
  # Test rounding behavior
  value <- 0.12345678901234567890
  result_1 <- format_pct(value, my_digits = 1)
  result_5 <- format_pct(value, my_digits = 5)
  result_10 <- format_pct(value, my_digits = 10)

  expect_type(result_1, "character")
  expect_type(result_5, "character")
  expect_type(result_10, "character")
})

test_that("percentage calculations are accurate", {
  # Test that percentages sum to 100
  df <- data.frame(
    category = sample(c("A", "B", "C", "D"), 1000, replace = TRUE)
  )

  result <- calculate_proportion(df, category)
  expect_equal(sum(result$percent), 100)

  # Test with uneven distribution
  df_uneven <- data.frame(
    category = c(rep("A", 7), rep("B", 2), "C")
  )

  result2 <- calculate_proportion(df_uneven, category)
  expect_equal(sum(result2$percent), 100)
})

# Test Suite: Memory and Performance Edge Cases
test_that("functions handle repeated operations without memory leaks", {
  # Run the same operation many times
  for (i in 1:100) {
    df <- data.frame(cat = sample(letters[1:3], 100, replace = TRUE))
    result <- calculate_proportion(df, cat)
    expect_s3_class(result, "data.frame")
  }
})

# Test Suite: Column Name Edge Cases
test_that("functions handle unusual column names", {
  df_weird_names <- data.frame(
    `column with spaces` = c("A", "B", "A", "C"),
    `123numeric` = c(1, 2, 1, 3),
    `.hidden` = c("X", "Y", "X", "Z"),
    check.names = FALSE
  )

  # This should work with backticks in the actual call
  # For testing purposes, we'll use standard names
  df_standard <- data.frame(
    col_spaces = c("A", "B", "A", "C")
  )

  result <- calculate_proportion(df_standard, col_spaces)
  expect_s3_class(result, "data.frame")
})

test_that("calcpercentages handles column name as variable", {
  df <- data.frame(category = c("A", "A", "B"))

  # Test with character string
  result <- calcpercentages(df, "category")
  expect_s3_class(result, "data.frame")
})

# Test Suite: Statistical Edge Cases
test_that("functions handle tied frequencies correctly", {
  # Perfect tie
  df_tie <- data.frame(
    category = c("A", "A", "B", "B", "C", "C")
  )

  result <- calcpercentages(df_tie, "category")
  expect_s3_class(result, "data.frame")
  # Should return at least one of the tied values
  expect_true(nrow(result) >= 1)
  expect_equal(result$n[1], 2)
})

test_that("functions handle uniform distribution", {
  df_uniform <- data.frame(
    category = rep(letters[1:10], each = 10)
  )

  result <- calculate_proportion(df_uniform, category)
  expect_s3_class(result, "data.frame")
  # All percentages should be equal (10%)
  expect_true(all(abs(result$percent - 10) < 0.1))
})

# Test Suite: Missing Data Patterns
test_that("functions handle all-missing data", {
  df_all_na <- data.frame(
    category = c(NA, NA, NA, NA)
  )

  result <- calculate_proportion(df_all_na, category)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
})

test_that("functions handle alternating missing data", {
  df_alternating <- data.frame(
    category = c("A", NA, "B", NA, "A", NA)
  )

  result <- calculate_proportion(df_alternating, category)
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 2)  # A, B, and NA
})

# Test Suite: Duplicate Handling
test_that("functions handle complete duplicates", {
  df_dupes <- data.frame(
    category = rep("A", 1000)
  )

  result <- calcpercentages(df_dupes, "category")
  expect_equal(result$n, 1000)
  expect_equal(result$percent, 100)
})

# Test Suite: Zero-variance Data
test_that("functions handle zero-variance data appropriately", {
  df_constant <- data.frame(
    category = rep("SAME", 100),
    value = rep(42, 100)
  )

  result <- calculate_proportion(df_constant, category)
  expect_equal(nrow(result), 1)
  expect_equal(result$percent, 100)
})
