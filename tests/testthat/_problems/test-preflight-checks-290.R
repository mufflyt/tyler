# Extracted from test-preflight-checks.R:290

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "mysterycall", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(testthat)
library(mysterycall)

# test -------------------------------------------------------------------------
temp_dir <- tempfile()
dir.create(temp_dir)
expect_error(
    mysterycall_preflight_check(
      input_data = list(first = "John", last = "Doe"),
      output_dir = temp_dir,
      check_apis = FALSE,
      interactive = FALSE
    ),
    "Preflight checks failed"
  )
