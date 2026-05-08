library(testthat)
testthat::skip_if_not_installed("dplyr")
testthat::skip_if_not_installed("censusapi")
library(dplyr)

mock_getCensus <- function(name, vintage, vars, region, regionin, key, ...) {
  data.frame(
    NAME = paste("Block Group", regionin),
    "block group" = "1",
    state = "01",
    county = "001",
    tract = "000100",
    B01001_001E = 500,
    B01001_026E = 250,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

test_that("retrieves Census data for valid FIPS codes", {
  with_mocked_bindings(
    getCensus = mock_getCensus,
    .package = "censusapi",
    code = {
      result <- get_census_data(c("01", "02"), api_key = "fake")
      expect_true(nrow(result) > 0)
      expect_true("name" %in% colnames(result))
    }
  )
})

test_that("handles empty FIPS list gracefully", {
  result <- get_census_data(character(), api_key = "fake")
  expect_equal(nrow(result), 0)
})

test_that("handles invalid FIPS codes gracefully", {
  with_mocked_bindings(
    getCensus = function(...) NULL,
    .package = "censusapi",
    code = {
      result <- get_census_data(c("ZZ", "XX"), api_key = "fake")
      expect_equal(nrow(result), 0)
    }
  )
})

test_that("validates input type", {
  expect_error(get_census_data(1:2, api_key = "fake"), "character vector")
})

test_that("requires API key", {
  expect_error(get_census_data(c("01"), api_key = ""), "Census API key")
})
