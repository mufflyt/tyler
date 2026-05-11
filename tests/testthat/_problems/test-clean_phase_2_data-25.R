# Extracted from test-clean_phase_2_data.R:25

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "mysterycall", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(testthat)
testthat::skip_if_not_installed("janitor")
testthat::skip_if_not_installed("readr")

# test -------------------------------------------------------------------------
raw <- data.frame(
    doctor_info = 1:2,
    doctor_notes = 3:4,
    contact_number = 5:6,
    stringsAsFactors = FALSE
  )
expect_warning(
    renamed <- rename_columns_by_substring(
      raw,
      target_strings = c("doctor", "contact"),
      new_names = c("physician", "contact_info")
    ),
    "Multiple columns match"
  )
