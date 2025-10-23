library(testthat)
testthat::skip_if_not_installed("dplyr")
testthat::skip_if_not_installed("mockery")
library(dplyr)
library(mockery)

mock_getCensus <- function(name, vintage, vars, region, regionin, key) {
  data.frame(
    NAME = paste("Block Group", regionin),
    B01001_001E = 500,
    B01001_026E = 250
  )
}

test_that("retrieves Census data for valid FIPS codes", {
  mockery::stub(get_census_data, 'censusapi::getCensus', mock_getCensus)
  result <- get_census_data(c("01", "02"), api_key = "fake")
  expect_true(nrow(result) > 0)
  expect_true("NAME" %in% colnames(result))
})

test_that("handles empty FIPS list gracefully", {
  result <- get_census_data(character(), api_key = "fake")
  expect_equal(nrow(result), 0)
})

test_that("handles invalid FIPS codes gracefully", {
  mockery::stub(get_census_data, 'censusapi::getCensus', function(...) NULL)
  result <- get_census_data(c("ZZ", "XX"), api_key = "fake")
  expect_equal(nrow(result), 0)
})

test_that("validates input type", {
  expect_error(get_census_data(1:2, api_key = "fake"), "character vector")
})

test_that("requires API key", {
  expect_error(get_census_data(c("01"), api_key = ""), "Census API key")
})
