library(testthat)
library(tyler)
library(dplyr)

# Unit Tests for calculate_proportion
test_that("calculate_proportion calculates basic proportions correctly", {
  data <- data.frame(gender = c("Male", "Female", "Female", "Male", "Male", "Female"))
  result <- calculate_proportion(data, gender)

  expect_s3_class(result, "data.frame")
  expect_true("n" %in% colnames(result))
  expect_true("percent" %in% colnames(result))
  expect_equal(nrow(result), 2)
  expect_equal(sum(result$n), 6)
  expect_equal(sum(result$percent), 100)
})

test_that("calculate_proportion handles missing values", {
  df_na <- data.frame(gender = c("Male", NA, "Female", "Female", "Male", "Female", NA))
  result <- calculate_proportion(df_na, gender)

  expect_s3_class(result, "data.frame")
  expect_true("n" %in% colnames(result))
  expect_true("percent" %in% colnames(result))
  # Should have 3 rows: Male, Female, NA
  expect_equal(nrow(result), 3)
})

test_that("calculate_proportion handles multiple levels", {
  df_multi <- data.frame(grade = c("A", "B", "A", "C", "B", "A", "C", "B"))
  result <- calculate_proportion(df_multi, grade)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)  # A, B, C
  expect_equal(sum(result$n), 8)
  expect_equal(sum(result$percent), 100)
})

test_that("calculate_proportion handles single level", {
  df_single <- data.frame(category = c("A", "A", "A", "A"))
  result <- calculate_proportion(df_single, category)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$n, 4)
  expect_equal(result$percent, 100)
})

test_that("calculate_proportion handles single row", {
  df_single_row <- data.frame(category = "A")
  result <- calculate_proportion(df_single_row, category)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$n, 1)
  expect_equal(result$percent, 100)
})

test_that("calculate_proportion rounds percentages to 2 decimal places", {
  df <- data.frame(category = c("A", "B", "C"))
  result <- calculate_proportion(df, category)

  expect_s3_class(result, "data.frame")
  # Each should be 33.33%
  expect_true(all(result$percent >= 33.33 & result$percent <= 33.34))
})

# Regression Tests
test_that("calculate_proportion handles all NA values", {
  df_all_na <- data.frame(category = c(NA, NA, NA))
  result <- calculate_proportion(df_all_na, category)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$n, 3)
})

test_that("calculate_proportion handles empty data frame", {
  df_empty <- data.frame(category = character(0))
  result <- calculate_proportion(df_empty, category)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("calculate_proportion handles numeric categories", {
  df_numeric <- data.frame(score = c(1, 2, 1, 3, 2, 1))
  result <- calculate_proportion(df_numeric, score)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_equal(sum(result$percent), 100)
})

test_that("calculate_proportion handles factor variables", {
  df_factor <- data.frame(level = factor(c("Low", "High", "Low", "Medium", "High", "Low")))
  result <- calculate_proportion(df_factor, level)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_equal(sum(result$percent), 100)
})

test_that("calculate_proportion percentages sum to 100", {
  df <- data.frame(category = sample(c("A", "B", "C", "D"), 100, replace = TRUE))
  result <- calculate_proportion(df, category)

  expect_equal(sum(result$percent), 100)
})

test_that("calculate_proportion handles large dataset", {
  df_large <- data.frame(category = sample(letters[1:5], 10000, replace = TRUE))
  result <- calculate_proportion(df_large, category)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 5)
  expect_equal(sum(result$n), 10000)
  expect_equal(sum(result$percent), 100)
})
