library(testthat)
testthat::skip_if_not_installed("dplyr")
testthat::skip_if_not_installed("janitor")
testthat::skip_if_not_installed("readr")
testthat::skip_if_not_installed("stringr")
testthat::skip_if_not_installed("humaniformat")
library(dplyr)
library(janitor)
library(readr)
library(stringr)
library(humaniformat)

test_that("clean_phase_1_results handles empty data frames correctly", {
  df_empty <- data.frame()
  expect_message(clean_phase_1_results(df_empty), "The data frame is empty. Exiting function.")
})

test_that("clean_phase_1_results stops if required columns are missing", {
  df_missing_cols <- data.frame(names = c("John Doe", "Jane Doe"), stringsAsFactors = FALSE)
  expect_error(clean_phase_1_results(df_missing_cols), "required columns are missing")
})

test_that("clean_phase_1_results processes data correctly", {
  df_normal <- data.frame(
    names = c("John Doe", "Jane Doe"),
    practice_name = c("Univ Hospital", "Private Clinic"),
    phone_number = c("1234567890", "0987654321"),
    state_name = c("TX", "CA"),
    npi = c(123456, 654321),
    stringsAsFactors = FALSE
  )
  result <- clean_phase_1_results(df_normal)

  expect_true(nrow(result) > 0)
  expect_true(all(c("id", "for_redcap") %in% names(result)))
})

test_that("clean_phase_1_results adds insurance correctly", {
  df_with_insurance <- data.frame(
    names = c("John Doe", "Jane Doe"),
    practice_name = c("Univ Hospital", "Private Clinic"),
    phone_number = c("1234567890", "0987654321"),
    state_name = c("TX", "CA"),
    npi = c(123456, 654321),
    stringsAsFactors = FALSE
  )
  result <- clean_phase_1_results(df_with_insurance)
  expected_insurance <- c("Blue Cross/Blue Shield", "Medicaid")

  expect_equal(result$insurance, expected_insurance)
})

test_that("clean_phase_1_results removes duplicates correctly", {
  df_dups <- data.frame(
    names = c("John Doe", "Jane Doe", "John Doe"),
    practice_name = c("Univ Hospital", "Private Clinic", "Univ Hospital"),
    phone_number = c("1234567890", "0987654321", "1234567890"),
    state_name = c("TX", "CA", "TX"),
    npi = c(123456, 654321, 123456),
    stringsAsFactors = FALSE
  )
  result <- clean_phase_1_results(df_dups)
  expect_equal(nrow(result), 2) # Expecting duplicates to be removed
})
