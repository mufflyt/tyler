library(testthat)
library(tyler)
library(dplyr)

# Unit Tests for calcpercentages
test_that("calcpercentages returns most common value correctly", {
  data_frame <- data.frame(category = c("A", "B", "A", "C", "A", "B", "B", "A"))
  result <- calcpercentages(data_frame, "category")

  expect_s3_class(result, "data.frame")
  expect_true("n" %in% colnames(result))
  expect_true("percent" %in% colnames(result))
  expect_equal(nrow(result), 1)
  expect_equal(result$category, "A")  # A appears 4 times
  expect_equal(result$n, 4)
  expect_equal(result$percent, 50)
})

test_that("calcpercentages handles ties in frequency", {
  df_tie <- data.frame(category = c("A", "B", "A", "B", "C"))
  result <- calcpercentages(df_tie, "category")

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 1)  # May return multiple if tied
  expect_true(result$n[1] == 2)  # Both A and B appear twice
})

test_that("calcpercentages handles missing values", {
  df_na <- data.frame(category = c("A", NA, "A", "C", "A", "B", "B", NA))
  result <- calcpercentages(df_na, "category")

  expect_s3_class(result, "data.frame")
  expect_true("n" %in% colnames(result))
  expect_true("percent" %in% colnames(result))
})

test_that("calcpercentages handles single unique value", {
  df_single <- data.frame(category = c("A", "A", "A", "A"))
  result <- calcpercentages(df_single, "category")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$category, "A")
  expect_equal(result$n, 4)
  expect_equal(result$percent, 100)
})

test_that("calcpercentages handles single row dataframe", {
  df_single_row <- data.frame(category = "A")
  result <- calcpercentages(df_single_row, "category")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$category, "A")
  expect_equal(result$n, 1)
  expect_equal(result$percent, 100)
})

test_that("calcpercentages calculates percentage correctly", {
  df <- data.frame(category = c("A", "A", "B", "C"))
  result <- calcpercentages(df, "category")

  expect_s3_class(result, "data.frame")
  expect_equal(result$n, 2)
  expect_equal(result$percent, 50)
})

# Regression Tests
test_that("calcpercentages handles numeric categories", {
  df_numeric <- data.frame(score = c(1, 2, 1, 3, 1))
  result <- calcpercentages(df_numeric, "score")

  expect_s3_class(result, "data.frame")
  expect_equal(result$n, 3)  # 1 appears 3 times
  expect_equal(result$percent, 60)
})

test_that("calcpercentages handles factor variables", {
  df_factor <- data.frame(level = factor(c("Low", "High", "Low", "Low", "High")))
  result <- calcpercentages(df_factor, "level")

  expect_s3_class(result, "data.frame")
  expect_equal(result$n, 3)  # Low appears 3 times
  expect_equal(result$percent, 60)
})

test_that("calcpercentages handles empty dataframe", {
  df_empty <- data.frame(category = character(0))
  result <- calcpercentages(df_empty, "category")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("calcpercentages handles all NA values", {
  df_all_na <- data.frame(category = c(NA, NA, NA))
  result <- calcpercentages(df_all_na, "category")

  expect_s3_class(result, "data.frame")
  # Should return NA as most common
  expect_equal(nrow(result), 1)
})

test_that("calcpercentages handles variable name as character", {
  df <- data.frame(category = c("A", "B", "A", "A"))
  result <- calcpercentages(df, "category")

  expect_s3_class(result, "data.frame")
  expect_equal(result$category, "A")
})

test_that("calcpercentages handles large dataset", {
  set.seed(123)
  df_large <- data.frame(category = sample(letters[1:5], 10000, replace = TRUE))
  result <- calcpercentages(df_large, "category")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true(result$n > 0)
  expect_true(result$percent > 0 && result$percent <= 100)
})

test_that("calcpercentages percentage calculation is accurate", {
  df <- data.frame(category = c("A", "A", "A", "B", "B", "C", "C", "C", "C", "C"))
  result <- calcpercentages(df, "category")

  # C appears 5 times out of 10
  expect_equal(result$category, "C")
  expect_equal(result$n, 5)
  expect_equal(result$percent, 50)
})
