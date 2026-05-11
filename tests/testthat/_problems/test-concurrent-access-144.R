# Extracted from test-concurrent-access.R:144

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "mysterycall", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(testthat)
library(mysterycall)
library(dplyr)

# test -------------------------------------------------------------------------
skip_on_cran()
test_data <- data.frame(
    names = paste("Dr.", rep(c("John Doe", "Mary Smith"), 50)),
    practice_name = paste("Practice", 1:100),
    phone_number = rep("555-123-4567", 100),
    state_name = sample(state.name, 100, replace = TRUE),
    stringsAsFactors = FALSE
  )
temp_dir <- tempfile()
dir.create(temp_dir)
results1 <- mysterycall_clean_phase1(
    phase1_data = test_data,
    output_directory = temp_dir,
    verbose = FALSE,
    notify = FALSE
  )
files_before <- list.files(temp_dir)
expect_no_error({
    # Simulate reading
    if (length(files_before) > 0) {
      file_path <- file.path(temp_dir, files_before[1])
      if (file.exists(file_path)) {
        existing_data <- read.csv(file_path)
      }
    }

    # Write again
    results2 <- mysterycall_clean_phase1(
      phase1_data = test_data,
      output_directory = temp_dir,
      verbose = FALSE,
      notify = FALSE
    )
  })
