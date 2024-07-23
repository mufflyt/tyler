library(testthat)

test_that("physician_age function works correctly", {
  # Create test data
  df <- data.frame(age = c(34, 50, 45, 60, 36, 29, 54, 43, 38, 48))

  # Calculate expected result
  median_age <- round(median(df$age, na.rm = TRUE), 2)
  q25 <- round(quantile(df$age, probs = 0.25, na.rm = TRUE), 1)
  q75 <- round(quantile(df$age, probs = 0.75, na.rm = TRUE), 1)
  expected <- paste0(
    "The median age of the dataset was ", median_age,
    " (IQR 25th percentile ", q25, " to 75th percentile ", q75, ")."
  )

  # Run test
  result <- physician_age(df, "age")
  expect_equal(result, expected)
})

test_that("physician_age handles NA values correctly", {
  # Create test data with NA values
  df <- data.frame(age = c(34, 50, 45, 60, NA, 29, 54, 43, 38, 48))

  # Calculate expected result
  median_age <- round(median(df$age, na.rm = TRUE), 2)
  q25 <- round(quantile(df$age, probs = 0.25, na.rm = TRUE), 1)
  q75 <- round(quantile(df$age, probs = 0.75, na.rm = TRUE), 1)
  expected <- paste0(
    "The median age of the dataset was ", median_age,
    " (IQR 25th percentile ", q25, " to 75th percentile ", q75, ")."
  )

  # Run test
  result <- physician_age(df, "age")
  expect_equal(result, expected)
})

test_that("physician_age handles an empty dataframe correctly", {
  # Create empty test data
  df <- data.frame(age = numeric(0))

  # Calculate expected result
  median_age <- round(median(df$age, na.rm = TRUE), 2)
  q25 <- round(quantile(df$age, probs = 0.25, na.rm = TRUE), 1)
  q75 <- round(quantile(df$age, probs = 0.75, na.rm = TRUE), 1)
  expected <- paste0(
    "The median age of the dataset was ", median_age,
    " (IQR 25th percentile ", q25, " to 75th percentile ", q75, ")."
  )

  # Run test
  result <- physician_age(df, "age")
  expect_equal(result, expected)
})
