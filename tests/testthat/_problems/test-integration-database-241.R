# Extracted from test-integration-database.R:241

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "mysterycall", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(testthat)
library(mysterycall)
library(dplyr)
create_test_census_db <- function() {
  list(
    "01" = list(  # Alabama
      "001" = data.frame(
        NAME = "Autauga County, Alabama",
        B01001_001E = 58805,  # Total population
        B01001_002E = 28952,  # Male
        B01001_026E = 29853,  # Female
        B01001_030E = 1234,   # Female 15-17
        B01001_031E = 1456,   # Female 18-19
        B01001_032E = 1678,   # Female 20
        stringsAsFactors = FALSE
      )
    ),
    "06" = list(  # California
      "075" = data.frame(
        NAME = "San Francisco County, California",
        B01001_001E = 884363,
        B01001_002E = 461428,
        B01001_026E = 422935,
        B01001_030E = 12234,
        B01001_031E = 14567,
        B01001_032E = 16789,
        stringsAsFactors = FALSE
      )
    )
  )
}
create_test_npi_db <- function() {
  data.frame(
    npi = c("1234567890", "0987654321", "1122334455", "5566778899"),
    basic_first_name = c("John", "Jane", "Michael", "Sarah"),
    basic_last_name = c("Smith", "Doe", "Johnson", "Williams"),
    basic_credential = c("MD", "DO", "MD", "MD"),
    basic_gender = c("M", "F", "M", "F"),
    addresses_state = c("CA", "TX", "NY", "FL"),
    addresses_city = c("San Francisco", "Houston", "New York", "Miami"),
    taxonomies_desc = c(
      "Obstetrics & Gynecology",
      "Gynecologic Oncology",
      "Maternal & Fetal Medicine",
      "Reproductive Endocrinology"
    ),
    addresses_country_name = rep("United States", 4),
    stringsAsFactors = FALSE
  )
}

# test -------------------------------------------------------------------------
skip_on_cran()
problematic_data <- data.frame(
    id = 1:6,
    names = c("Valid Name", "", NA, "Name123", "!@#$%", "Very Very Very Long Name That Exceeds Normal Expectations"),
    practice_name = c("Valid Practice", "", NA, "P", "Practice 123", "Valid Practice 2"),
    phone_number = c("555-1234", "invalid", "", NA, "123", "555-555-5555"),
    state_name = c("California", "CA", "", NA, "InvalidState", "TX"),
    npi = c("1234567890", "invalid", "123", NA, "12345678901", "0987654321"),
    for_redcap = c("Yes", "No", "", NA, "Maybe", "Yes"),
    stringsAsFactors = FALSE
  )
temp_dir <- tempdir()
result <- mysterycall_clean_phase1(
    phase1_data = problematic_data,
    output_directory = temp_dir,
    verbose = FALSE,
    notify = FALSE
  )
expect_s3_class(result, "data.frame")
expect_true(all(!is.na(result$names)))
