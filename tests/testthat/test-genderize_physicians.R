library(testthat)
library(readr)
library(dplyr)
library(gender)
library(fs)
library(mockery)

# Create a temporary CSV file for testing
create_temp_csv <- function(data, file_name = "temp_physicians.csv") {
  temp_file <- file.path(tempdir(), file_name)
  write_csv(data, temp_file)
  return(temp_file)
}

# Sample data for testing
sample_data <- data.frame(
  first_name = c("John", "Jane", "Alex", "Sam", "Chris"),
  last_name = c("Doe", "Smith", "Brown", "Wilson", "Taylor"),
  stringsAsFactors = FALSE
)

# Test if the function reads the input file correctly
test_that("Reads input file correctly", {
  temp_csv <- create_temp_csv(sample_data)

  result <- genderize_physicians(temp_csv, tempdir())

  expect_true(file.exists(temp_csv))
  expect_equal(nrow(result), nrow(sample_data))
  expect_equal(ncol(result), ncol(sample_data) + 1) # original columns + gender column
})

# Test if the function genderizes the first names correctly
test_that("Genderizes first names correctly", {
  temp_csv <- create_temp_csv(sample_data)

  result <- genderize_physicians(temp_csv, tempdir())

  expect_true("gender" %in% colnames(result))
  expect_false(any(is.na(result$gender)))
})

# Test if the function saves the result to a new CSV file with a timestamp
test_that("Saves result to new CSV file with timestamp", {
  temp_csv <- create_temp_csv(sample_data)

  genderize_physicians(temp_csv, tempdir())

  output_files <- list.files(tempdir(), pattern = "^genderized_.*\\.csv$")
  expect_true(length(output_files) > 0)

  output_path <- file.path(tempdir(), output_files[1])
  result_data <- read_csv(output_path, show_col_types = FALSE)

  expect_equal(nrow(result_data), nrow(sample_data))
  expect_equal(ncol(result_data), ncol(sample_data) + 1) # original columns + gender column
})

# Test if the function handles missing first names gracefully
test_that("Handles missing first names gracefully", {
  sample_data_with_na <- sample_data
  sample_data_with_na$first_name[1] <- NA
  temp_csv <- create_temp_csv(sample_data_with_na)

  result <- genderize_physicians(temp_csv, tempdir())

  expect_true("gender" %in% colnames(result))
  expect_true(any(is.na(result$gender)))
})

# Test with mixed case names
test_that("Handles mixed case names correctly", {
  mixed_case_data <- data.frame(
    first_name = c("john", "JANE", "aLex", "sam", "CHRIS"),
    last_name = c("Doe", "Smith", "Brown", "Wilson", "Taylor"),
    stringsAsFactors = FALSE
  )
  temp_csv <- create_temp_csv(mixed_case_data)

  result <- genderize_physicians(temp_csv, tempdir())

  expect_true("gender" %in% colnames(result))
  expect_false(any(is.na(result$gender)))
})

# Test with additional columns
test_that("Handles additional columns correctly", {
  additional_columns_data <- data.frame(
    first_name = c("John", "Jane", "Alex", "Sam", "Chris"),
    last_name = c("Doe", "Smith", "Brown", "Wilson", "Taylor"),
    age = c(30, 25, 40, 35, 50),
    city = c("New York", "Los Angeles", "Chicago", "Houston", "Phoenix"),
    stringsAsFactors = FALSE
  )
  temp_csv <- create_temp_csv(additional_columns_data)

  result <- genderize_physicians(temp_csv, tempdir())

  expect_true("gender" %in% colnames(result))
  expect_false(any(is.na(result$gender)))
  expect_equal(ncol(result), ncol(additional_columns_data) + 1) # original columns + gender column
})

# Test with mixed case names
test_that("Handles mixed case names correctly", {
  mixed_case_data <- data.frame(
    first_name = c("john", "JANE", "aLex", "sam", "CHRIS"),
    last_name = c("Doe", "Smith", "Brown", "Wilson", "Taylor"),
    stringsAsFactors = FALSE
  )
  temp_csv <- create_temp_csv(mixed_case_data)

  result <- genderize_physicians(temp_csv, tempdir())

  expect_true("gender" %in% colnames(result))
  expect_false(any(is.na(result$gender)))
})
