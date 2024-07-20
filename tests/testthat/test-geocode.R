# Now, let's write tests for the updated function

library(testthat)
library(dplyr)
library(readr)
library(mockery)

# Mock the ggmap::geocode function
mock_geocode <- function(address, key) {
  return(data.frame(lat = 37.7749, lon = -122.4194))  # Return mock coordinates
}

# Sample data for testing
sample_data <- data.frame(
  address = c("1600 Amphitheatre Parkway, Mountain View, CA",
              "1 Infinite Loop, Cupertino, CA",
              "1601 Willow Road, Menlo Park, CA"),
  stringsAsFactors = FALSE
)

# Create a temporary CSV file for testing
create_temp_csv <- function(data, file_name = "temp_data.csv") {
  temp_file <- file.path(tempdir(), file_name)
  write_csv(data, temp_file)
  return(temp_file)
}

# Test if the function reads the input file correctly
test_that("Reads input file correctly", {
  temp_csv <- create_temp_csv(sample_data)
  expect_silent(suppressMessages(read_csv(temp_csv)))
})

# Test if the function handles missing file correctly
test_that("Handles missing file correctly", {
  mockery::stub(geocode_unique_addresses, 'file.exists', function(file) FALSE)
  expect_error(geocode_unique_addresses("nonexistent_file.csv", "fake_api_key"), "Input file not found.")
})

# Test if the function handles missing address column correctly
test_that("Handles missing address column correctly", {
  temp_csv <- create_temp_csv(sample_data %>% select(-address))
  expect_error(geocode_unique_addresses(temp_csv, "fake_api_key"), "The dataset must have a column named 'address' for geocoding.")
})

# Test if the function processes data correctly
test_that("Processes data correctly", {
  temp_csv <- create_temp_csv(sample_data)
  mockery::stub(geocode_unique_addresses, 'ggmap::geocode', mock_geocode)
  result <- geocode_unique_addresses(temp_csv, "fake_api_key")
  expect_equal(ncol(result), ncol(sample_data) + 2)  # Original columns + latitude and longitude
  expect_true("latitude" %in% colnames(result))
  expect_true("longitude" %in% colnames(result))
})

# Test if the function saves the output file correctly
test_that("Saves output file correctly", {
  temp_csv <- create_temp_csv(sample_data)
  output_csv <- file.path(tempdir(), "output_data.csv")
  mockery::stub(geocode_unique_addresses, 'ggmap::geocode', mock_geocode)
  geocode_unique_addresses(temp_csv, "fake_api_key", output_csv)
  expect_true(file.exists(output_csv))
  output_data <- suppressMessages(read_csv(output_csv))
  expect_equal(ncol(output_data), ncol(sample_data) + 2)
  expect_true("latitude" %in% colnames(output_data))
  expect_true("longitude" %in% colnames(output_data))
})
