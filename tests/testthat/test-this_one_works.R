library(testthat)
testthat::skip_if_not_installed("mockery")
testthat::skip_if_not_installed("httr")
testthat::skip_if_not_installed("jsonlite")
testthat::skip_if_not_installed("dplyr")
testthat::skip_if_not_installed("readr")
library(mockery)
library(httr)
library(jsonlite)
library(dplyr)
library(readr)

# Corrected mock functions
mock_get <- function(url, ...) {
  cat("Mock GET request to URL:", url, "\n")
  list(status_code = if (grepl("verify", url)) 200 else 404)
}

mock_content <- function(...) {
  cat("Mock content called\n")
  '{"name":"Dr. John Doe"}'
}

mock_fromJSON <- function(text, ...) {
  cat("Mock fromJSON called\n")
  return(if (nchar(text) > 0) list(name = "Dr. John Doe") else list())
}

mock_read_csv <- function(...) {
  cat("Mock read_csv called\n")
  return(data.frame(WrongIDs = c(9045998, 9045999)))
}

# Test cases
test_that("Handles wrong IDs correctly", {
  cat("Running test: Handles wrong IDs correctly\n")
  stub(scrape_physicians_data_with_tor, 'httr::GET', mock_get)
  stub(scrape_physicians_data_with_tor, 'httr::content', mock_content)
  stub(scrape_physicians_data_with_tor, 'jsonlite::fromJSON', mock_fromJSON)
  stub(scrape_physicians_data_with_tor, 'readr::read_csv', mock_read_csv)

  result <- scrape_physicians_data_with_tor(9045998, 9045999, 9150)

  expect_equal(nrow(result), 0)
})
