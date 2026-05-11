# Extracted from test-property-based.R:419

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
input_sizes <- c(10, 50, 100)
memory_usage <- numeric(length(input_sizes))
for (i in seq_along(input_sizes)) {
    test_data <- generate_test_dataframe(input_sizes[i], valid_only = TRUE)
    temp_dir <- tempdir()

    # Measure memory usage
    gc()
    mem_before <- sum(gc()[, 2])

    result <- mysterycall_clean_phase1(
      phase1_data = test_data,
      output_directory = temp_dir,
      verbose = FALSE,
      notify = FALSE
    )

    gc()
    mem_after <- sum(gc()[, 2])
    memory_usage[i] <- mem_after - mem_before
  }
for (i in 2:length(input_sizes)) {
    size_ratio <- input_sizes[i] / input_sizes[i-1]
    memory_ratio <- memory_usage[i] / memory_usage[i-1]

    expect_lt(memory_ratio, size_ratio^1.5,
              label = paste("Memory scaling too steep between size",
                          input_sizes[i-1], "and", input_sizes[i]))
  }
