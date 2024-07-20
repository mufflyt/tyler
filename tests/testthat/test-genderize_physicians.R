library(testthat)
library(readr)
library(dplyr)
library(gender)
library(fs)

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

  result <- genderize_physicians(temp_csv)

  expect_true(file.exists(temp_csv))
  expect_equal(nrow(result), nrow(sample_data))
  expect_equal(ncol(result), ncol(sample_data) + 1) # original columns + gender column
})

# Test if the function genderizes the first names correctly
test_that("Genderizes first names correctly", {
  temp_csv <- create_temp_csv(sample_data)

  result <- genderize_physicians(temp_csv)

  expect_true("gender" %in% colnames(result))
  expect_false(any(is.na(result$gender)))
})

# Test if the function saves the result to a new CSV file with a timestamp
test_that("Saves result to new CSV file with timestamp", {
  temp_csv <- create_temp_csv(sample_data)

  genderize_physicians(temp_csv)

  output_files <- list.files(tempdir(), pattern = "^genderized_.*\\.csv$")
  expect_true(length(output_files) > 0)

  output_path <- file.path(tempdir(), output_files[1])
  result_data <- read_csv(output_path)

  expect_equal(nrow(result_data), nrow(sample_data))
  expect_equal(ncol(result_data), ncol(sample_data) + 1) # original columns + gender column
})

# Test if the function handles missing first names gracefully
test_that("Handles missing first names gracefully", {
  sample_data_with_na <- sample_data
  sample_data_with_na$first_name[1] <- NA
  temp_csv <- create_temp_csv(sample_data_with_na)

  result <- genderize_physicians(temp_csv)

  expect_true("gender" %in% colnames(result))
  expect_true(any(is.na(result$gender)))
})

# Run the tests
test_dir(".")
