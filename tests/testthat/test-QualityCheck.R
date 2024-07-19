library(testthat)
library(dplyr)

# Sample data to use in tests
sample_data <- data.frame(
  npi = c(123, 123, 123, 456, 456, 456, 789),
  name = c("John Doe", "John Doe", "John Doe", "Jane Doe", "Jane Doe", "Alice Smith", "Alice Smith")
)

# Define the file path for testing (temporary directory)
test_filepath <- tempfile(fileext = ".csv")

# Test to check file creation
test_that("Output file is created", {
  save_quality_check_table(sample_data, test_filepath)
  expect_true(file.exists(test_filepath))
})

# Cleanup after tests
unlink(test_filepath)  # Remove the temporary file
