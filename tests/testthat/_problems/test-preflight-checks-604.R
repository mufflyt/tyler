# Extracted from test-preflight-checks.R:604

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "mysterycall", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(testthat)
library(mysterycall)

# test -------------------------------------------------------------------------
single_row <- data.frame(
    first = "John",
    last = "Doe",
    stringsAsFactors = FALSE
  )
temp_dir <- tempfile()
dir.create(temp_dir)
result <- mysterycall_preflight_check(
    input_data = single_row,
    output_dir = temp_dir,
    check_apis = FALSE,
    interactive = FALSE
  )
