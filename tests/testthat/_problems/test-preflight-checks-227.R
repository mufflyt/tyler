# Extracted from test-preflight-checks.R:227

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "mysterycall", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(testthat)
library(mysterycall)

# test -------------------------------------------------------------------------
test_data <- data.frame(
    first = c("John", "Jane", "Bob"),
    last = c("Doe", "Smith", "Jones"),
    stringsAsFactors = FALSE
  )
temp_file <- tempfile(fileext = ".csv")
temp_dir <- tempfile()
dir.create(temp_dir)
write.csv(test_data, temp_file, row.names = FALSE)
result <- mysterycall_preflight_check(
    input_data = temp_file,
    output_dir = temp_dir,
    check_apis = FALSE,
    interactive = FALSE
  )
