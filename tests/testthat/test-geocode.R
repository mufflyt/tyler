library(testthat)
testthat::skip_if_not_installed("dplyr")
testthat::skip_if_not_installed("readr")
testthat::skip_if_not_installed("ggmap")
library(dplyr)
library(readr)

mock_geocode <- function(address, key, ...) {
  data.frame(lat = 37.7749, lon = -122.4194)
}

sample_data <- data.frame(
  address = c(
    "1600 Amphitheatre Parkway, Mountain View, CA",
    "1 Infinite Loop, Cupertino, CA",
    "1601 Willow Road, Menlo Park, CA"
  ),
  stringsAsFactors = FALSE
)

create_temp_csv <- function(data, file_name = "temp_data.csv") {
  temp_file <- file.path(tempdir(), file_name)
  write_csv(data, temp_file)
  temp_file
}

test_that("Reads input file correctly", {
  temp_csv <- create_temp_csv(sample_data)
  expect_silent(suppressMessages(read_csv(temp_csv)))
})

test_that("Handles missing file correctly", {
  # Use a path that truly does not exist — no stub needed
  expect_error(
    geocode_unique_addresses(
      file.path(tempdir(), "does_not_exist_xyz123.csv"),
      "fake_api_key",
      notify = FALSE
    ),
    "Input file not found."
  )
})

test_that("Handles missing address column correctly", {
  temp_csv <- create_temp_csv(sample_data %>% select(-"address"))
  expect_error(
    geocode_unique_addresses(temp_csv, "fake_api_key", notify = FALSE),
    "The dataset must have a column named 'address' for geocoding."
  )
})

test_that("Processes data correctly", {
  temp_csv <- create_temp_csv(sample_data)
  with_mocked_bindings(
    geocode = mock_geocode,
    .package = "ggmap",
    code = {
      result <- geocode_unique_addresses(temp_csv, "fake_api_key", notify = FALSE)
      expect_equal(ncol(result), ncol(sample_data) + 2)
      expect_true("latitude" %in% colnames(result))
      expect_true("longitude" %in% colnames(result))
    }
  )
})

test_that("Saves output file correctly", {
  temp_csv <- create_temp_csv(sample_data)
  output_csv <- file.path(tempdir(), "output_data.csv")
  with_mocked_bindings(
    geocode = mock_geocode,
    .package = "ggmap",
    code = {
      geocode_unique_addresses(temp_csv, "fake_api_key", output_csv, notify = FALSE)
      expect_true(file.exists(output_csv))
      output_data <- suppressMessages(read_csv(output_csv))
      expect_equal(ncol(output_data), ncol(sample_data) + 2)
      expect_true("latitude" %in% colnames(output_data))
      expect_true("longitude" %in% colnames(output_data))
    }
  )
})


test_that("Handles partial geocode response without crashing", {
  temp_csv <- create_temp_csv(sample_data)
  short_geocode <- function(address, key, ...) {
    data.frame(lat = c(37.7749, 37.3318), lon = c(-122.4194, -122.0312))
  }

  with_mocked_bindings(
    geocode = short_geocode,
    .package = "ggmap",
    code = {
      expect_warning(
        result <- geocode_unique_addresses(temp_csv, "fake_api_key", notify = FALSE),
        "Missing rows will be padded with NA coordinates"
      )
      expect_equal(nrow(result), nrow(sample_data))
      expect_true(any(is.na(result$latitude) | is.na(result$longitude)))
    }
  )
})
