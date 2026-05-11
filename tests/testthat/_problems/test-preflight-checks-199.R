# Extracted from test-preflight-checks.R:199

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "mysterycall", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(testthat)
library(mysterycall)

# test -------------------------------------------------------------------------
test_data <- data.frame(
    first = c("John", "Jane"),
    last = c("Doe", "Smith"),
    stringsAsFactors = FALSE
  )
temp_dir <- tempfile()
dir.create(temp_dir)
result <- mysterycall_preflight_check(
    input_data = test_data,
    output_dir = temp_dir,
    check_apis = FALSE,
    interactive = FALSE
  )
