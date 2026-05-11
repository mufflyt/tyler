# Extracted from test-preflight-checks.R:469

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "mysterycall", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(testthat)
library(mysterycall)

# test -------------------------------------------------------------------------
test_data <- data.frame(
    first = c("John"),
    last = c("Doe"),
    stringsAsFactors = FALSE
  )
temp_dir <- tempfile()
dir.create(temp_dir)
result <- mysterycall_preflight_check(
    input_data = test_data,
    output_dir = temp_dir,
    google_maps_api_key = NULL,
    here_api_key = NULL,
    check_apis = FALSE,
    interactive = FALSE
  )
