# Extracted from test-preflight-checks.R:629

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "mysterycall", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(testthat)
library(mysterycall)

# test -------------------------------------------------------------------------
bad_data <- data.frame(
    wrong_col1 = c(NA, NA, NA),
    wrong_col2 = c(NA, NA, NA),
    stringsAsFactors = FALSE
  )
expect_error(
    mysterycall_preflight_check(
      input_data = bad_data,
      output_dir = "/invalid/path/that/cannot/be/created",
      required_columns = c("first", "last"),
      check_apis = FALSE,
      interactive = FALSE
    ),
    "Preflight checks failed"
  )
