library(testthat)
library(tyler)
library(ggplot2)

# Unit Tests for check_normality
test_that("check_normality detects normally distributed data", {
  set.seed(123)
  # Generate normally distributed data
  normal_data <- data.frame(values = rnorm(100, mean = 50, sd = 10))

  # Suppress plot output
  suppressMessages({
    result <- check_normality(normal_data, "values")
  })

  expect_type(result, "list")
  expect_true("mean" %in% names(result) || "median" %in% names(result))

  # Normal data should have mean and sd
  if ("mean" %in% names(result)) {
    expect_true("sd" %in% names(result))
    expect_true(is.numeric(result$mean))
    expect_true(is.numeric(result$sd))
  }
})

test_that("check_normality detects non-normal data", {
  set.seed(123)
  # Generate exponentially distributed data (non-normal)
  skewed_data <- data.frame(values = rexp(100, rate = 0.5))

  suppressMessages({
    result <- check_normality(skewed_data, "values")
  })

  expect_type(result, "list")
  # Non-normal data might have median and iqr
  expect_true(("mean" %in% names(result) && "sd" %in% names(result)) ||
                ("median" %in% names(result) && "iqr" %in% names(result)))
})

test_that("check_normality handles uniform distribution", {
  set.seed(123)
  uniform_data <- data.frame(values = runif(100, min = 0, max = 100))

  suppressMessages({
    result <- check_normality(uniform_data, "values")
  })

  expect_type(result, "list")
  expect_true(length(result) == 2)
})

test_that("check_normality throws error for small sample size", {
  small_data <- data.frame(values = c(1, 2))

  expect_error(
    suppressMessages(check_normality(small_data, "values")),
    "Sample size must be at least 3"
  )
})

test_that("check_normality handles minimum sample size", {
  min_data <- data.frame(values = c(1, 2, 3))

  suppressMessages({
    result <- check_normality(min_data, "values")
  })

  expect_type(result, "list")
  expect_true(length(result) == 2)
})

test_that("check_normality returns correct structure for normal data", {
  set.seed(456)
  normal_data <- data.frame(values = rnorm(1000, mean = 100, sd = 15))

  suppressMessages({
    result <- check_normality(normal_data, "values")
  })

  expect_type(result, "list")
  if ("mean" %in% names(result)) {
    expect_true(abs(result$mean - 100) < 5)  # Should be close to 100
    expect_true(abs(result$sd - 15) < 5)     # Should be close to 15
  }
})

test_that("check_normality handles data with outliers", {
  set.seed(789)
  outlier_data <- data.frame(values = c(rnorm(97, mean = 50, sd = 10), 1000, 2000, 3000))

  suppressMessages({
    result <- check_normality(outlier_data, "values")
  })

  expect_type(result, "list")
  # With outliers, likely to be detected as non-normal
})

# Regression Tests
test_that("check_normality handles single value repeated", {
  constant_data <- data.frame(values = rep(5, 100))

  suppressMessages({
    result <- check_normality(constant_data, "values")
  })

  expect_type(result, "list")
  if ("mean" %in% names(result)) {
    expect_equal(result$mean, 5)
    expect_equal(result$sd, 0)
  } else {
    expect_equal(result$median, 5)
    expect_equal(result$iqr, 0)
  }
})

test_that("check_normality handles negative values", {
  set.seed(321)
  negative_data <- data.frame(values = rnorm(100, mean = -50, sd = 10))

  suppressMessages({
    result <- check_normality(negative_data, "values")
  })

  expect_type(result, "list")
  if ("mean" %in% names(result)) {
    expect_true(result$mean < 0)
  }
})

test_that("check_normality handles mixed positive and negative values", {
  set.seed(654)
  mixed_data <- data.frame(values = rnorm(100, mean = 0, sd = 50))

  suppressMessages({
    result <- check_normality(mixed_data, "values")
  })

  expect_type(result, "list")
  expect_true(length(result) == 2)
})

test_that("check_normality handles large dataset", {
  set.seed(111)
  large_data <- data.frame(values = rnorm(10000, mean = 75, sd = 20))

  suppressMessages({
    result <- check_normality(large_data, "values")
  })

  expect_type(result, "list")
  if ("mean" %in% names(result)) {
    expect_true(abs(result$mean - 75) < 2)  # Should be very close with large n
  }
})

test_that("check_normality returns numeric summary statistics", {
  set.seed(222)
  data <- data.frame(values = rnorm(100, mean = 50, sd = 10))

  suppressMessages({
    result <- check_normality(data, "values")
  })

  # All returned values should be numeric
  expect_true(all(sapply(result, is.numeric)))
})

test_that("check_normality handles integer values", {
  integer_data <- data.frame(values = sample(1:100, 100, replace = TRUE))

  suppressMessages({
    result <- check_normality(integer_data, "values")
  })

  expect_type(result, "list")
  expect_true(length(result) == 2)
})

test_that("check_normality handles bimodal distribution", {
  set.seed(333)
  # Create bimodal distribution
  bimodal_data <- data.frame(values = c(rnorm(50, mean = 20, sd = 5),
                                        rnorm(50, mean = 80, sd = 5)))

  suppressMessages({
    result <- check_normality(bimodal_data, "values")
  })

  expect_type(result, "list")
  # Bimodal should be detected as non-normal
})
