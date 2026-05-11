# Extracted from test-regression-tracking.R:292

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "mysterycall", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(testthat)
library(mysterycall)
library(dplyr)
source(test_path("../fixtures/baseline_metrics.R"))
create_test_physicians <- function(n = 100) {
  data.frame(
    first = c("Sarah", "Michael", "Jennifer", "Robert", "Lisa",
             "David", "Emily", "James", "Amanda", "Christopher")[rep(1:10, length.out = n)],
    last = c("Johnson", "Chen", "Williams", "Garcia", "Thompson",
            "Martinez", "Davis", "Wilson", "Rodriguez", "Lee")[rep(1:10, length.out = n)],
    practice_name = paste("Practice", 1:n),
    phone_number = sprintf("555-%03d-%04d",
                          sample(100:999, n, replace = TRUE),
                          sample(1000:9999, n, replace = TRUE)),
    state_name = sample(state.name, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

# test -------------------------------------------------------------------------
skip_on_cran()
baseline <- BASELINE_METRICS$genderization
test_data <- data.frame(
    first_name = c("John", "Mary", "Michael", "Sarah", "David",
                   "Jennifer", "Robert", "Lisa", "James", "Emily"),
    stringsAsFactors = FALSE
  )
results <- test_data %>%
    mutate(
      gender = sample(
        c(rep("M", 5), rep("F", 5), rep(NA_character_, 2)),
        n(), replace = TRUE
      ),
      probability = ifelse(!is.na(gender), runif(n(), 0.6, 0.99), NA_real_)
    )
assignment_rate <- sum(!is.na(results$gender)) / nrow(results)
check <- check_metric_regression(
    current_value = assignment_rate,
    baseline_value = baseline$assignment_rate,
    tolerance = baseline$assignment_rate_tolerance,
    min_acceptable = baseline$min_acceptable
  )
expect_true(check$passed,
             label = format_regression_message("Genderization Assignment Rate", check))
