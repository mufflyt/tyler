# Extracted from test-clean_phase_1_results.R:48

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "mysterycall", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
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

# test -------------------------------------------------------------------------
df_with_insurance <- data.frame(
    names = c("John Doe", "Jane Doe"),
    practice_name = c("Univ Hospital", "Private Clinic"),
    phone_number = c("1234567890", "0987654321"),
    state_name = c("TX", "CA"),
    npi = c(123456, 654321),
    stringsAsFactors = FALSE
  )
result <- mysterycall_clean_phase1(df_with_insurance, duplicate_rows = FALSE, verbose = FALSE)
expect_setequal(result$insurance, c("Blue Cross/Blue Shield", "Medicaid"))
