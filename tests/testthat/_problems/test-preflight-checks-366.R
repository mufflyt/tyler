# Extracted from test-preflight-checks.R:366

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "mysterycall", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(testthat)
library(mysterycall)

# test -------------------------------------------------------------------------
poor_data <- data.frame(
    first = c(NA, NA, NA, NA, NA),
    last = c(NA, NA, NA, NA, NA),
    stringsAsFactors = FALSE
  )
temp_dir <- tempfile()
dir.create(temp_dir)
expect_error(
    mysterycall_preflight_check(
      input_data = poor_data,
      output_dir = temp_dir,
      check_apis = FALSE,
      interactive = FALSE
    ),
    "Preflight checks failed"
  )
