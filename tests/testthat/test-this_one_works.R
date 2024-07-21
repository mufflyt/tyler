library(testthat)
library(mockery)
library(httr)
library(jsonlite)
library(dplyr)
library(readr)

# Corrected mock functions
mock_get <- function(url, ...) {
  cat("Mock GET request to URL:", url, "\n")
  if (grepl("verify", url)) {
    response <- list(status_code = 200, content = function(...) '{"name":"Dr. John Doe"}')
  } else {
    response <- list(status_code = 404, content = function(...) '')
  }
  class(response) <- "response"
  return(response)
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
  stub(scrape_physicians_data_with_tor, 'jsonlite::fromJSON', mock_fromJSON)
  stub(scrape_physicians_data_with_tor, 'readr::read_csv', mock_read_csv)

  result <- scrape_physicians_data_with_tor(9045998, 9045999, 9150)

  expect_equal(nrow(result), 0)
})

test_that("Handles failed requests", {
  cat("Running test: Handles failed requests\n")
  stub(scrape_physicians_data_with_tor, 'httr::GET', function(...) {
    response <- list(status_code = 404, content = function(...) '')
    class(response) <- "response"
    return(response)
  })
  stub(scrape_physicians_data_with_tor, 'jsonlite::fromJSON', mock_fromJSON)
  stub(scrape_physicians_data_with_tor, 'readr::read_csv', mock_read_csv)

  result <- scrape_physicians_data_with_tor(9045999, 9046000, 9150)

  expect_equal(nrow(result), 0)
})
