# Extracted from test-sanity-checks-gold-standard.R:33

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "mysterycall", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(testthat)

# test -------------------------------------------------------------------------
df <- data.frame(x = seq_len(1000))
expect_warning(
    mysterycall_check_no_limits(df, "api_result"),
    "SUSPICIOUS"
  )
