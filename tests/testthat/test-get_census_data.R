library(testthat)
library(mockery)

mock_getCensus <- function(name, vintage, vars, region, regionin, key) {
  data.frame(
    NAME = paste("Block Group", regionin),
    B01001_001E = 1,
    B01001_026E = 2
  )
}

test_that("Retrieves Census data for valid FIPS codes", {
  old <- options(tyler.quiet = TRUE)
  on.exit(options(old), add = TRUE)
  stub(get_census_data, 'censusapi::getCensus', mock_getCensus)
  result <- get_census_data(c("1", "02"), vintage = 2022, api_key = "key", quiet = TRUE)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
})

test_that("Empty FIPS list returns empty tibble", {
  result <- get_census_data(character(0), api_key = "key", quiet = TRUE)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("Non atomic input triggers error", {
  expect_error(get_census_data(list("01"), api_key = "key"), "atomic vector")
})

test_that("Missing FIPS values trigger error", {
  expect_error(get_census_data(c("01", NA), api_key = "key"), "missing or empty")
})

test_that("Invalid vintage triggers error", {
  expect_error(get_census_data("01", vintage = c(2020, 2021), api_key = "key"), "single numeric")
})

test_that("Missing API key triggers error", {
  expect_error(get_census_data("01", api_key = ""), "Census API key required")
})

test_that("Handles API failures gracefully", {
  old <- options(tyler.quiet = TRUE)
  on.exit(options(old), add = TRUE)
  stub(get_census_data, 'censusapi::getCensus', function(...) stop("boom"))
  result <- get_census_data("01", api_key = "key", quiet = TRUE)
  expect_equal(nrow(result), 0)
})
