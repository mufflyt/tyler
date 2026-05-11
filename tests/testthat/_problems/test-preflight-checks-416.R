# Extracted from test-preflight-checks.R:416

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "mysterycall", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(testthat)
library(mysterycall)

# test -------------------------------------------------------------------------
test_data <- data.frame(
    first = rep("John", 100),
    last = rep("Doe", 100),
    stringsAsFactors = FALSE
  )
temp_dir <- tempfile()
dir.create(temp_dir)
result <- mysterycall_preflight_check(
    input_data = test_data,
    output_dir = temp_dir,
    check_apis = FALSE,
    estimate_resources = TRUE,
    interactive = FALSE
  )
