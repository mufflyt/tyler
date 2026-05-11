# Extracted from test-preflight-checks.R:654

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "mysterycall", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(testthat)
library(mysterycall)

# test -------------------------------------------------------------------------
large_data <- data.frame(
    first = rep(c("John", "Jane", "Bob"), 1000),
    last = rep(c("Doe", "Smith", "Jones"), 1000),
    stringsAsFactors = FALSE
  )
temp_dir <- tempfile()
dir.create(temp_dir)
start_time <- Sys.time()
result <- mysterycall_preflight_check(
    input_data = large_data,
    output_dir = temp_dir,
    check_apis = FALSE,
    interactive = FALSE
  )
