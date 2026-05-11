# Extracted from test-clean_phase_2_data.R:99

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "mysterycall", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(testthat)
testthat::skip_if_not_installed("janitor")
testthat::skip_if_not_installed("readr")

# test -------------------------------------------------------------------------
tmp_dir <- tempfile("phase2-regression-")
dir.create(tmp_dir)
on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
raw <- data.frame(
    PhysicianInformation = c("Jane Doe", "John Smith"),
    AbleToContactOffice = c("Yes", "No"),
    AreWeIncluding = c("Yes", "No"),
    ReasonForExclusions = c(NA, "Closed"),
    AppointmentDate = c("2024-01-01", "2024-01-02"),
    NumberOfTransfers = c(0, 1),
    CallTime = c(120, 95),
    HoldTime = c(30, 40),
    Notes = c("Scheduled", "Try again"),
    PersonCompleting = c("Caller A", "Caller B"),
    stringsAsFactors = FALSE
  )
required_strings <- c(
    "physician_information",
    "able_to_contact_office",
    "are_we_including",
    "reason_for_exclusions",
    "appointment_date",
    "number_of_transfers",
    "call_time",
    "hold_time",
    "notes",
    "person_completing"
  )
standard_names <- c(
    "physician_info",
    "contact_office",
    "included_in_study",
    "exclusion_reasons",
    "appt_date",
    "transfer_count",
    "call_duration",
    "hold_duration",
    "caller_notes",
    "completed_by"
  )
expected <- data.frame(
    physician_info = raw$PhysicianInformation,
    contact_office = raw$AbleToContactOffice,
    included_in_study = raw$AreWeIncluding,
    exclusion_reasons = raw$ReasonForExclusions,
    appt_date = raw$AppointmentDate,
    transfer_count = raw$NumberOfTransfers,
    call_duration = raw$CallTime,
    hold_duration = raw$HoldTime,
    caller_notes = raw$Notes,
    completed_by = raw$PersonCompleting,
    stringsAsFactors = FALSE
  )
cleaned <- clean_phase_2_data(
    raw,
    required_strings,
    standard_names,
    output_directory = tmp_dir
  )
expect_equal(as.data.frame(cleaned), expected)
