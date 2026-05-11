# Extracted from test-property-based.R:480

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "mysterycall", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(testthat)
library(mysterycall)
library(dplyr)
generate_valid_npi <- function(n = 1) {
  paste0(sample(100000000:999999999, n), sample(0:9, n, replace = TRUE))
}
generate_valid_phone <- function(n = 1) {
  formats <- c(
    "%03d-%03d-%04d",
    "(%03d) %03d-%04d",
    "%03d.%03d.%04d"
  )

  sapply(1:n, function(i) {
    format <- sample(formats, 1)
    sprintf(format,
            sample(200:999, 1),
            sample(200:999, 1),
            sample(1000:9999, 1))
  })
}
generate_valid_state <- function(n = 1) {
  sample(c(state.name, state.abb), n, replace = TRUE)
}
generate_test_dataframe <- function(n = 10, valid_only = FALSE) {
  if (valid_only) {
    data.frame(
      id = 1:n,
      names = paste("Provider", 1:n),
      practice_name = paste("Practice", sample(1:min(50, n), n, replace = TRUE)),
      phone_number = generate_valid_phone(n),
      state_name = generate_valid_state(n),
      npi = generate_valid_npi(n),
      for_redcap = sample(c("Yes", "No"), n, replace = TRUE),
      stringsAsFactors = FALSE
    )
  } else {
    # Include some invalid data
    data.frame(
      id = 1:n,
      names = c(paste("Provider", 1:(n-2)), "", NA),
      practice_name = c(paste("Practice", 1:(n-3)), "", NA, "Valid"),
      phone_number = c(generate_valid_phone(n-3), "invalid", "", NA),
      state_name = c(generate_valid_state(n-3), "Invalid", "", NA),
      npi = c(generate_valid_npi(n-3), "invalid", "", NA),
      for_redcap = c(rep("Yes", n-2), "", NA),
      stringsAsFactors = FALSE
    )
  }
}

# test -------------------------------------------------------------------------
skip_on_cran()
perfect_data <- data.frame(
    id = 1:20,
    names = paste("Dr.", sample(c("John", "Jane", "Michael", "Sarah"), 20, replace = TRUE),
                  sample(c("Smith", "Johnson", "Williams", "Brown"), 20, replace = TRUE)),
    practice_name = paste(sample(c("Medical", "Health", "Family", "Women's"), 20, replace = TRUE),
                         sample(c("Center", "Clinic", "Associates", "Group"), 20, replace = TRUE)),
    phone_number = generate_valid_phone(20),
    state_name = sample(state.name, 20, replace = TRUE),
    npi = generate_valid_npi(20),
    for_redcap = sample(c("Yes", "No"), 20, replace = TRUE),
    stringsAsFactors = FALSE
  )
temp_dir <- tempdir()
cleaned_data <- mysterycall_clean_phase1(
    phase1_data = perfect_data,
    output_directory = temp_dir,
    verbose = FALSE,
    notify = FALSE,
    duplicate_rows = FALSE
  )
validated_data <- mysterycall_validate_npi(cleaned_data)
expect_s3_class(validated_data, "data.frame")
expect_equal(nrow(validated_data), nrow(perfect_data))
