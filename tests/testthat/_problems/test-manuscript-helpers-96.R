# Extracted from test-manuscript-helpers.R:96

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "mysterycall", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
skip_if_not_installed("lme4")
set.seed(1)
df <- data.frame(
    wait  = rpois(40, 14),
    ins   = rep(c("BCBS", "Medicaid"), 20),
    phys  = rep(paste0("D", 1:8), 5),
    stringsAsFactors = FALSE
  )
res <- mysterycall_poisson_model(df, "wait", "ins", "phys")
expect_error(
    mysterycall_write_results_paragraph(res, "BCBS", "gender"),
    "No coefficient"
  )
