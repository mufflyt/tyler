library(testthat)

skip_if_not_installed("openxlsx")

test_that("run_mystery_caller_workflow orchestrates pipeline", {
  tmp <- tempdir()
  phase1_out <- file.path(tmp, "phase1")
  splits_dir <- file.path(tmp, "splits")
  qc_path <- file.path(tmp, "qc", "quality.csv")

  phase1_data <- data.frame(
    names = c("Jane Doe", "John Smith"),
    practice_name = c("Front Range OBGYN", "Rocky Mountain Health"),
    phone_number = c("303-555-0100", "720-555-0199"),
    state_name = c("Colorado", "Colorado"),
    npi = c("1111111116", "2222222224"),
    stringsAsFactors = FALSE
  )

  phase2_data <- data.frame(
    physician_information = c("Jane Doe", "John Smith"),
    able_to_contact_office = c("Yes", "No"),
    are_we_including = c("Yes", "No"),
    reason_for_exclusions = c(NA_character_, "Closed"),
    appointment_date = as.Date(c("2024-01-01", "2024-01-02")),
    number_of_transfers = c(0, 1),
    call_time = c(120, 95),
    hold_time = c(30, 40),
    notes = c("Scheduled", "Try again"),
    person_completing = c("Caller A", "Caller B"),
    state = c("Colorado", "Colorado"),
    npi = c("1111111116", "2222222224"),
    name = c("Jane Doe", "John Smith"),
    stringsAsFactors = FALSE
  )

  result <- run_mystery_caller_workflow(
    taxonomy_terms = NULL,
    name_data = data.frame(first = character(), last = character()),
    phase1_data = phase1_data,
    lab_assistant_names = c("Alpha", "Bravo"),
    output_directory = splits_dir,
    phase2_data = phase2_data,
    quality_check_path = qc_path,
    phase1_output_directory = phase1_out
  )

  expect_type(result, "list")
  expect_true(all(c("roster", "validated_roster", "cleaned_phase1", "cleaned_phase2",
                    "coverage_summary", "quality_check_table") %in% names(result)))

  expect_true(dir.exists(phase1_out))
  expect_s3_class(result$cleaned_phase1, "data.frame")
  expect_true(all(c("doctor_id", "for_redcap") %in% names(result$cleaned_phase1)))

  split_files <- list.files(splits_dir, pattern = "\\.xlsx$", full.names = TRUE)
  expect_equal(length(split_files), 3L)

  expect_s3_class(result$cleaned_phase2, "data.frame")
  expect_type(result$coverage_summary, "character")

  expect_true(file.exists(qc_path))
  expect_s3_class(result$quality_check_table, "data.frame")
})
