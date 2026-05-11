# Extracted from test-preflight-checks.R:273

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "mysterycall", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(testthat)
library(mysterycall)

# test -------------------------------------------------------------------------
bad_data <- data.frame(
    name = c("John Doe", "Jane Smith"),
    phone = c("555-1234", "555-5678"),
    stringsAsFactors = FALSE
  )
temp_dir <- tempfile()
dir.create(temp_dir)
expect_error(
    mysterycall_preflight_check(
      input_data = bad_data,
      output_dir = temp_dir,
      required_columns = c("first", "last"),
      check_apis = FALSE,
      interactive = FALSE
    ),
    "Preflight checks failed"
  )
