library(testthat)
library(readr)
library(dplyr)
library(mockery)
library(purrr)
library(tidyr)

# Create a temporary CSV file for testing
create_temp_csv <- function(data, file_name = "temp_npi_data.csv") {
  temp_file <- file.path(tempdir(), file_name)
  write_csv(data, temp_file)
  return(temp_file)
}

# Sample data for testing
sample_data_valid <- data.frame(
  npi = c(1234567890, 2345678901, 3456789012, 4567890123, 5678901234),
  stringsAsFactors = FALSE
)

sample_data_invalid <- data.frame(
  npi = c("12345", "23456789012", "ABCDE12345", NA, ""),
  stringsAsFactors = FALSE
)

sample_data_mixed <- data.frame(
  npi = c(1234567890, "23456789012", 3456789012, NA, ""),
  stringsAsFactors = FALSE
)

# Mock function to simulate provider::clinicians
mock_clinicians <- function(npi) {
  if (npi == 1234567890) {
    return(data.frame(name = "John Doe", specialty = "Cardiology"))
  } else if (npi == 3456789012) {
    return(data.frame(name = "Jane Smith", specialty = "Neurology"))
  } else {
    return(NULL)
  }
}

# Mock function to validate and remove invalid NPIs
mock_validate_and_remove_invalid_npi <- function(df) {
  df <- df %>%
    dplyr::filter(npi %in% c(1234567890, 3456789012, "1234567890", "3456789012")) %>%
    dplyr::mutate(npi = as.character(npi), npi_is_valid = TRUE)
  return(df)
}

# Tests
test_that("Retrieves clinician data for valid NPIs", {
  temp_csv <- create_temp_csv(sample_data_valid)
  mockery::stub(retrieve_clinician_data, 'provider::clinicians', mock_clinicians)
  mockery::stub(retrieve_clinician_data, 'validate_and_remove_invalid_npi', mock_validate_and_remove_invalid_npi)
  result <- retrieve_clinician_data(temp_csv)
  expect_equal(nrow(result), 2) # Only valid NPIs with clinician data should remain
  expect_true("name" %in% colnames(result))
  expect_true("specialty" %in% colnames(result))
  expect_equal(result$name, c("John Doe", "Jane Smith"))
})

test_that("Handles invalid NPIs", {
  temp_csv <- create_temp_csv(sample_data_invalid)
  mockery::stub(retrieve_clinician_data, 'provider::clinicians', mock_clinicians)
  mockery::stub(retrieve_clinician_data, 'validate_and_remove_invalid_npi', mock_validate_and_remove_invalid_npi)
  result <- retrieve_clinician_data(temp_csv)
  expect_equal(nrow(result), 0) # No valid NPIs should remain
})

test_that("Handles mixed valid and invalid NPIs", {
  temp_csv <- create_temp_csv(sample_data_mixed)
  mockery::stub(retrieve_clinician_data, 'provider::clinicians', mock_clinicians)
  mockery::stub(retrieve_clinician_data, 'validate_and_remove_invalid_npi', mock_validate_and_remove_invalid_npi)
  result <- retrieve_clinician_data(temp_csv)
  expect_equal(nrow(result), 2) # Only valid NPIs with clinician data should remain
  expect_true("name" %in% colnames(result))
  expect_true("specialty" %in% colnames(result))
  expect_equal(result$name, c("John Doe", "Jane Smith"))
})

test_that("Handles dataframe input correctly", {
  mockery::stub(retrieve_clinician_data, 'provider::clinicians', mock_clinicians)
  mockery::stub(retrieve_clinician_data, 'validate_and_remove_invalid_npi', mock_validate_and_remove_invalid_npi)
  result <- retrieve_clinician_data(sample_data_mixed)
  expect_equal(nrow(result), 2) # Only valid NPIs with clinician data should remain
  expect_true("name" %in% colnames(result))
  expect_true("specialty" %in% colnames(result))
  expect_equal(result$name, c("John Doe", "Jane Smith"))
})
