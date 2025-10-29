# Tests for Comprehensive Logging System
# Testing workflow tracking, logging functions, and output formatting

library(testthat)
library(tyler)

# ==============================================================================
# tyler_format_duration()
# ==============================================================================

test_that("tyler_format_duration formats seconds correctly", {
  expect_match(tyler_format_duration(5.5), "5\\.5s")
  expect_match(tyler_format_duration(45), "45\\.0s")
  expect_match(tyler_format_duration(59), "59\\.0s")
})

test_that("tyler_format_duration formats minutes correctly", {
  expect_match(tyler_format_duration(60), "1m 0s")
  expect_match(tyler_format_duration(90), "1m 30s")
  expect_match(tyler_format_duration(3599), "59m 59s")
})

test_that("tyler_format_duration formats hours correctly", {
  expect_match(tyler_format_duration(3600), "1h 0m 0s")
  expect_match(tyler_format_duration(3665), "1h 1m 5s")
  expect_match(tyler_format_duration(7320), "2h 2m 0s")
})

test_that("tyler_format_duration handles edge cases", {
  expect_match(tyler_format_duration(0), "0\\.0s")
  expect_match(tyler_format_duration(0.1), "0\\.1s")
  expect_no_error(tyler_format_duration(100000))
})

# ==============================================================================
# tyler_workflow_start() and tyler_workflow_end()
# ==============================================================================

test_that("tyler_workflow_start initializes tracking", {
  expect_message(
    tyler_workflow_start("Test Workflow", total_steps = 3),
    "Test Workflow"
  )

  # Check that environment was initialized
  expect_true(exists("name", envir = tyler:::.tyler_workflow))
  expect_equal(tyler:::.tyler_workflow$name, "Test Workflow")
  expect_equal(tyler:::.tyler_workflow$total_steps, 3)
})

test_that("tyler_workflow_start accepts optional log file", {
  log_file <- tempfile(fileext = ".txt")

  expect_message(
    tyler_workflow_start("Test", log_file = log_file)
  )

  # Log file should be created
  expect_true(file.exists(log_file))

  # Cleanup
  tyler_workflow_end()
  unlink(log_file)
})

test_that("tyler_workflow_end prints summary", {
  tyler_workflow_start("Test Workflow", total_steps = 2)

  expect_message(
    tyler_workflow_end(),
    "COMPLETE"
  )
})

test_that("tyler_workflow_end handles final counts", {
  tyler_workflow_start("Test Workflow")

  expect_message(
    tyler_workflow_end(final_n = 95, input_n = 100),
    "95\\.1%"
  )
})

# ==============================================================================
# tyler_log_step() and tyler_log_step_complete()
# ==============================================================================

test_that("tyler_log_step logs step information", {
  tyler_workflow_start("Test", total_steps = 3)

  expect_message(
    tyler_log_step("Step 1", detail = "Processing data"),
    "Step 1"
  )

  tyler_workflow_end()
})

test_that("tyler_log_step_complete calculates timing", {
  tyler_workflow_start("Test", total_steps = 2)

  tyler_log_step("Quick Step")
  Sys.sleep(0.1)  # Small delay

  expect_message(
    tyler_log_step_complete(),
    "complete"
  )

  tyler_workflow_end()
})

test_that("tyler_log_step_complete shows success rates", {
  tyler_workflow_start("Test")

  tyler_log_step("Processing")

  expect_message(
    tyler_log_step_complete(n_success = 90, n_total = 100),
    "90/100"
  )

  tyler_workflow_end()
})

# ==============================================================================
# tyler_log_info(), tyler_log_success(), tyler_log_warning(), tyler_log_error()
# ==============================================================================

test_that("tyler_log_info outputs information", {
  expect_message(
    tyler_log_info("Processing started"),
    "Processing started"
  )
})

test_that("tyler_log_success outputs success message", {
  expect_message(
    tyler_log_success("Operation completed"),
    "Operation completed"
  )
})

test_that("tyler_log_success handles details", {
  details <- list(
    "Records processed" = "1000",
    "Time taken" = "5 minutes"
  )

  expect_message(
    tyler_log_success("Completed", details = details),
    "Records processed"
  )
})

test_that("tyler_log_warning outputs warnings", {
  expect_message(
    tyler_log_warning("Low success rate"),
    "WARNING"
  )
})

test_that("tyler_log_warning suggests fixes", {
  expect_message(
    tyler_log_warning("Issue detected", fix = "Try increasing timeout"),
    "Fix: Try increasing timeout"
  )
})

test_that("tyler_log_error outputs errors", {
  expect_message(
    tyler_log_error("Operation failed"),
    "ERROR"
  )
})

test_that("tyler_log_error includes cause and fix", {
  expect_message(
    tyler_log_error("Failed", cause = "Network timeout", fix = "Check connection"),
    "Cause: Network timeout"
  )
})

# ==============================================================================
# tyler_log_progress()
# ==============================================================================

test_that("tyler_log_progress shows progress", {
  expect_message(
    tyler_log_progress(50, 100),
    "50/100"
  )
})

test_that("tyler_log_progress calculates percentages", {
  expect_message(
    tyler_log_progress(75, 100, show_percent = TRUE),
    "75\\.0%"
  )
})

test_that("tyler_log_progress includes status", {
  expect_message(
    tyler_log_progress(30, 100, status = "Processing item 30"),
    "Processing item 30"
  )
})

# ==============================================================================
# tyler_log_cache_hit(), tyler_log_save()
# ==============================================================================

test_that("tyler_log_cache_hit logs cache usage", {
  expect_message(
    tyler_log_cache_hit("previous results", n_items = 500),
    "cache"
  )
})

test_that("tyler_log_save logs file saves", {
  expect_message(
    tyler_log_save("/path/to/output.csv", n_rows = 1000),
    "output\\.csv"
  )
})

# ==============================================================================
# tyler_progress_callback()
# ==============================================================================

test_that("tyler_progress_callback creates functional callback", {
  callback <- tyler_progress_callback(100, "Test")

  expect_type(callback, "closure")

  # Should not error when called
  expect_message(
    callback(10),
    "10/100"
  )
})

test_that("tyler_progress_callback updates periodically", {
  callback <- tyler_progress_callback(100, "Test")

  # First call (10%) should report
  expect_message(callback(10))

  # Small incremental calls shouldn't report
  expect_silent(callback(11))
  expect_silent(callback(12))

  # Next 10% threshold (20%) should report
  expect_message(callback(20))
})

# ==============================================================================
# Log File Writing
# ==============================================================================

test_that("Logging writes to file when specified", {
  log_file <- tempfile(fileext = ".txt")

  tyler_workflow_start("Test Workflow", log_file = log_file)
  tyler_log_info("Test message")
  tyler_log_success("Success message")
  tyler_workflow_end()

  # File should exist and contain messages
  expect_true(file.exists(log_file))

  log_content <- readLines(log_file)
  expect_true(length(log_content) > 0)
  expect_true(any(grepl("Test Workflow", log_content)))

  unlink(log_file)
})

test_that("Log file handles special characters", {
  log_file <- tempfile(fileext = ".txt")

  tyler_workflow_start("Test", log_file = log_file)
  tyler_log_info("Testing Unicode: ✓ ✗ ⚠")
  tyler_workflow_end()

  # File should be created
  expect_true(file.exists(log_file))

  unlink(log_file)
})

# ==============================================================================
# Integration Tests
# ==============================================================================

test_that("Complete workflow logging works end-to-end", {
  log_file <- tempfile(fileext = ".txt")

  # Start workflow
  tyler_workflow_start("Complete Test", total_steps = 3, log_file = log_file)

  # Step 1
  tyler_log_step("Load Data", n_items = 100)
  Sys.sleep(0.1)
  tyler_log_step_complete(n_success = 100, n_total = 100)

  # Step 2
  tyler_log_step("Process Data")
  tyler_log_progress(50, 100)
  tyler_log_progress(100, 100)
  tyler_log_step_complete(success_rate = 0.95)

  # Step 3
  tyler_log_step("Save Results")
  tyler_log_save("/tmp/output.csv", n_rows = 95)
  tyler_log_step_complete()

  # End workflow
  tyler_workflow_end(final_n = 95, input_n = 100)

  # Verify log file
  expect_true(file.exists(log_file))
  log_content <- readLines(log_file)
  expect_true(length(log_content) > 10)

  unlink(log_file)
})

test_that("Logging handles errors gracefully", {
  tyler_workflow_start("Error Test")

  tyler_log_step("Risky Operation")

  expect_message(
    tyler_log_error("Operation failed", cause = "Bad input", fix = "Check data"),
    "ERROR"
  )

  tyler_workflow_end()
})

test_that("Nested logging works correctly", {
  tyler_workflow_start("Outer Workflow", total_steps = 2)

  tyler_log_step("Outer Step 1")
  tyler_log_info("Starting inner operations")
  tyler_log_info("Inner operation 1")
  tyler_log_info("Inner operation 2")
  tyler_log_success("Inner operations complete")
  tyler_log_step_complete()

  tyler_workflow_end()
})

# ==============================================================================
# Edge Cases
# ==============================================================================

test_that("Logging handles very long messages", {
  long_message <- paste(rep("A", 1000), collapse = "")

  expect_message(
    tyler_log_info(long_message),
    "A+"
  )
})

test_that("Logging handles special characters in file paths", {
  tyler_log_save("/path/with spaces/and-dashes/file_name.csv", n_rows = 100)

  # Should not error
  expect_true(TRUE)
})

test_that("Logging handles NULL and NA values", {
  # These should not crash
  expect_silent(tyler_log_info("Test", indent = TRUE))
  expect_silent(tyler_log_success("Test", details = NULL))
  expect_silent(tyler_log_warning("Test", fix = NULL))
  expect_silent(tyler_log_error("Test", cause = NULL, fix = NULL))
})

# ==============================================================================
# Performance Tests
# ==============================================================================

test_that("Logging is performant", {
  start_time <- Sys.time()

  tyler_workflow_start("Performance Test", total_steps = 100)

  for (i in 1:100) {
    tyler_log_step(sprintf("Step %d", i))
    tyler_log_step_complete()
  }

  tyler_workflow_end()

  end_time <- Sys.time()
  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))

  # Should complete in reasonable time (< 2 seconds for 100 steps)
  expect_true(elapsed < 2.0)
})
