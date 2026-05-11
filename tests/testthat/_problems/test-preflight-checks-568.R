# Extracted from test-preflight-checks.R:568

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "mysterycall", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(testthat)
library(mysterycall)

# test -------------------------------------------------------------------------
test_data <- data.frame(
    first = c("John", "Jane", "Bob", "Alice", "Charlie"),
    last = c("Doe", "Smith", "Jones", "Williams", "Brown"),
    phone = c("555-1234", "555-5678", NA, "555-9012", "555-3456"),
    stringsAsFactors = FALSE
  )
temp_dir <- tempfile()
dir.create(temp_dir)
result <- mysterycall_preflight_check(
    input_data = test_data,
    output_dir = temp_dir,
    google_maps_api_key = "dummy_key",
    here_api_key = "dummy_key",
    check_apis = FALSE,
    estimate_resources = TRUE,
    interactive = FALSE
  )
