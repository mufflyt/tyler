library(testthat)
library(readr)

sample_df <- data.frame(
  npi = c("1234567890", " 123-456-7890 ", "ABCDEFGHIJ", NA, ""),
  stringsAsFactors = FALSE
)

test_that("validates numeric and character NPIs", {
  result <- validate_and_remove_invalid_npi(sample_df)
  expect_equal(nrow(result), 2)
  expect_true(all(result$npi == c("1234567890", "1234567890")))
  expect_true(all(result$npi_is_valid))
})

test_that("reads from csv with preserved digits", {
  temp_file <- tempfile(fileext = ".csv")
  write_csv(sample_df, temp_file)
  result <- validate_and_remove_invalid_npi(temp_file)
  expect_equal(result$npi, rep("1234567890", 2))
})

test_that("errors when npi column missing", {
  df <- data.frame(id = 1:2)
  expect_error(validate_and_remove_invalid_npi(df), "'npi' column")
})

test_that("returns empty tibble when no valid NPIs", {
  df <- data.frame(npi = c("111", "ABC"))
  result <- validate_and_remove_invalid_npi(df)
  expect_equal(nrow(result), 0)
  expect_true("npi_is_valid" %in% names(result))
})
