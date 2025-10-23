library(testthat)
library(dplyr)
library(janitor)
library(readr)
library(stringr)
library(humaniformat)

test_that("clean_phase_1_results handles empty data frames correctly", {
  df_empty <- data.frame()
  expect_output(clean_phase_1_results(df_empty, verbose = TRUE), "No data to process.")
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

  expect_equal(nrow(result), nrow(df_normal) * 2)
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
  result <- clean_phase_1_results(df_with_insurance, duplicate_rows = FALSE, verbose = FALSE)
  expect_setequal(result$insurance, c("Blue Cross/Blue Shield", "Medicaid"))
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
  result <- clean_phase_1_results(df_dups, duplicate_rows = FALSE, verbose = FALSE)
  expect_equal(nrow(result), nrow(df_dups))
})

test_that("clean_phase_1_results random IDs are reproducible with a seed", {
  df_seed <- data.frame(
    names = "John Doe",
    practice_name = "Univ Hospital",
    phone_number = "1234567890",
    state_name = "TX",
    npi = NA,
    stringsAsFactors = FALSE
  )

  result_1 <- clean_phase_1_results(df_seed, duplicate_rows = FALSE, verbose = FALSE, id_seed = 42)
  result_2 <- clean_phase_1_results(df_seed, duplicate_rows = FALSE, verbose = FALSE, id_seed = 42)

  expect_equal(result_1$random_id, result_2$random_id)
})
