library(testthat)
library(tyler)
library(dplyr)
library(mockery)

# Unit Tests for validate_and_remove_invalid_npi
test_that("validate_and_remove_invalid_npi accepts dataframe input", {
  # Create test data with valid NPI format (10 digits)
  test_df <- data.frame(
    npi = c(1234567890, 9876543210, 1111111111),
    name = c("Dr. A", "Dr. B", "Dr. C"),
    stringsAsFactors = FALSE
  )

  # Mock the npi_is_valid function to return TRUE for all NPIs
  stub(validate_and_remove_invalid_npi, 'npi::npi_is_valid', function(x) TRUE)

  result <- validate_and_remove_invalid_npi(test_df)

  expect_s3_class(result, "data.frame")
  expect_true("npi_is_valid" %in% colnames(result))
  expect_equal(nrow(result), 3)
})

test_that("validate_and_remove_invalid_npi removes missing NPIs", {
  test_df <- data.frame(
    npi = c(1234567890, NA, 9876543210),
    name = c("Dr. A", "Dr. B", "Dr. C"),
    stringsAsFactors = FALSE
  )

  stub(validate_and_remove_invalid_npi, 'npi::npi_is_valid', function(x) TRUE)

  result <- validate_and_remove_invalid_npi(test_df)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_false(any(is.na(result$npi)))
})

test_that("validate_and_remove_invalid_npi removes empty NPIs", {
  test_df <- data.frame(
    npi = c(1234567890, "", 9876543210),
    name = c("Dr. A", "Dr. B", "Dr. C"),
    stringsAsFactors = FALSE
  )

  stub(validate_and_remove_invalid_npi, 'npi::npi_is_valid', function(x) TRUE)

  result <- validate_and_remove_invalid_npi(test_df)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_false(any(result$npi == ""))
})

test_that("validate_and_remove_invalid_npi removes invalid NPIs", {
  test_df <- data.frame(
    npi = c(1234567890, 9876543210, 1111111111),
    name = c("Dr. A", "Dr. B", "Dr. C"),
    stringsAsFactors = FALSE
  )

  # Mock to return FALSE for second NPI
  mock_validator <- function(x) {
    if (x == "9876543210") return(FALSE)
    return(TRUE)
  }

  stub(validate_and_remove_invalid_npi, 'npi::npi_is_valid', mock_validator)

  result <- validate_and_remove_invalid_npi(test_df)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_false(9876543210 %in% result$npi)
})

test_that("validate_and_remove_invalid_npi handles NPIs with wrong length", {
  test_df <- data.frame(
    npi = c(1234567890, 123, 9876543210),  # 123 has wrong length
    name = c("Dr. A", "Dr. B", "Dr. C"),
    stringsAsFactors = FALSE
  )

  stub(validate_and_remove_invalid_npi, 'npi::npi_is_valid', function(x) TRUE)

  result <- validate_and_remove_invalid_npi(test_df)

  expect_s3_class(result, "data.frame")
  # Should remove the one with wrong length
  expect_equal(nrow(result), 2)
})

test_that("validate_and_remove_invalid_npi adds npi_is_valid column", {
  test_df <- data.frame(
    npi = c(1234567890, 9876543210),
    name = c("Dr. A", "Dr. B"),
    stringsAsFactors = FALSE
  )

  stub(validate_and_remove_invalid_npi, 'npi::npi_is_valid', function(x) TRUE)

  result <- validate_and_remove_invalid_npi(test_df)

  expect_true("npi_is_valid" %in% colnames(result))
  expect_true(all(result$npi_is_valid))
})

# Regression Tests
test_that("validate_and_remove_invalid_npi handles all invalid NPIs", {
  test_df <- data.frame(
    npi = c(123, 456, 789),
    name = c("Dr. A", "Dr. B", "Dr. C"),
    stringsAsFactors = FALSE
  )

  stub(validate_and_remove_invalid_npi, 'npi::npi_is_valid', function(x) FALSE)

  result <- validate_and_remove_invalid_npi(test_df)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("validate_and_remove_invalid_npi handles all missing NPIs", {
  test_df <- data.frame(
    npi = c(NA, NA, NA),
    name = c("Dr. A", "Dr. B", "Dr. C"),
    stringsAsFactors = FALSE
  )

  result <- validate_and_remove_invalid_npi(test_df)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("validate_and_remove_invalid_npi handles empty dataframe", {
  test_df <- data.frame(
    npi = numeric(0),
    name = character(0),
    stringsAsFactors = FALSE
  )

  result <- validate_and_remove_invalid_npi(test_df)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("validate_and_remove_invalid_npi throws error for invalid input type", {
  expect_error(
    validate_and_remove_invalid_npi(123),
    "Input must be a dataframe or a file path to a CSV"
  )
})

test_that("validate_and_remove_invalid_npi throws error for list input", {
  expect_error(
    validate_and_remove_invalid_npi(list(npi = c(1, 2, 3))),
    "Input must be a dataframe or a file path to a CSV"
  )
})

test_that("validate_and_remove_invalid_npi preserves other columns", {
  test_df <- data.frame(
    npi = c(1234567890, 9876543210),
    name = c("Dr. A", "Dr. B"),
    specialty = c("Cardiology", "Neurology"),
    city = c("New York", "Boston"),
    stringsAsFactors = FALSE
  )

  stub(validate_and_remove_invalid_npi, 'npi::npi_is_valid', function(x) TRUE)

  result <- validate_and_remove_invalid_npi(test_df)

  expect_true("name" %in% colnames(result))
  expect_true("specialty" %in% colnames(result))
  expect_true("city" %in% colnames(result))
  expect_equal(result$name, c("Dr. A", "Dr. B"))
})

test_that("validate_and_remove_invalid_npi handles mixed valid and invalid", {
  test_df <- data.frame(
    npi = c(1234567890, 9876543210, 1111111111, 2222222222),
    name = c("Dr. A", "Dr. B", "Dr. C", "Dr. D"),
    stringsAsFactors = FALSE
  )

  # Mock to return TRUE for first and third, FALSE for second and fourth
  mock_validator <- function(x) {
    x %in% c("1234567890", "1111111111")
  }

  stub(validate_and_remove_invalid_npi, 'npi::npi_is_valid', mock_validator)

  result <- validate_and_remove_invalid_npi(test_df)

  expect_equal(nrow(result), 2)
  expect_true(1234567890 %in% result$npi)
  expect_true(1111111111 %in% result$npi)
})

test_that("validate_and_remove_invalid_npi handles large dataset", {
  set.seed(123)
  test_df <- data.frame(
    npi = rep(1234567890, 1000),
    name = paste0("Dr. ", 1:1000),
    stringsAsFactors = FALSE
  )

  stub(validate_and_remove_invalid_npi, 'npi::npi_is_valid', function(x) TRUE)

  result <- validate_and_remove_invalid_npi(test_df)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1000)
})
