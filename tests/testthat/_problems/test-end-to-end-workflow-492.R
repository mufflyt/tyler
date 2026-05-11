# Extracted from test-end-to-end-workflow.R:492

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "mysterycall", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(testthat)
library(mysterycall)
library(dplyr)
library(readr)
create_realistic_phase1_data <- function() {
  # Based on actual mystery caller study structure
  data.frame(
    id = 1:25,
    names = c(
      "Dr. Sarah Johnson", "Dr. Michael Chen", "Dr. Jennifer Williams",
      "Dr. Robert Garcia", "Dr. Lisa Thompson", "Dr. David Martinez",
      "Dr. Emily Davis", "Dr. James Wilson", "Dr. Amanda Rodriguez",
      "Dr. Christopher Lee", "Dr. Rachel Brown", "Dr. Matthew Anderson",
      "Dr. Nicole Taylor", "Dr. Kevin Clark", "Dr. Stephanie Lewis",
      "Dr. Thomas Hall", "Dr. Michelle Young", "Dr. Daniel Allen",
      "Dr. Lauren King", "Dr. Andrew Wright", "Dr. Samantha Scott",
      "Dr. Richard Green", "Dr. Jessica Adams", "Dr. Joseph Baker",
      "Dr. Ashley Nelson"
    ),
    practice_name = c(
      "Women's Health Associates", "University Medical Center", "Family Care Clinic",
      "Regional Medical Group", "Downtown Health Center", "Northside OB/GYN",
      "Metro Women's Clinic", "Central Valley Health", "Suburban Medical",
      "City General Practice", "Westside Healthcare", "East End Medical",
      "Southside Women's Health", "Highland Medical Group", "Valley OB/GYN",
      "Riverside Healthcare", "Mountain View Clinic", "Lakeside Medical",
      "Sunset Women's Center", "Sunrise Healthcare", "Midtown Medical",
      "Crossroads Health", "Gateway Medical", "Pinnacle OB/GYN",
      "Heritage Women's Health"
    ),
    phone_number = c(
      "(555) 123-4567", "555-234-5678", "555.345.6789", "5554567890",
      "(555) 567-8901", "555-678-9012", "555.789.0123", "5558901234",
      "(555) 901-2345", "555-012-3456", "555.123.4567", "5552345678",
      "(555) 345-6789", "555-456-7890", "555.567.8901", "5556789012",
      "(555) 789-0123", "555-890-1234", "555.901.2345", "5550123456",
      "(555) 234-5678", "555-345-6789", "555.456.7890", "5557890123",
      "(555) 890-1234"
    ),
    state_name = c(
      "California", "Texas", "Florida", "New York", "Pennsylvania",
      "Illinois", "Ohio", "Georgia", "North Carolina", "Michigan",
      "New Jersey", "Virginia", "Washington", "Arizona", "Massachusetts",
      "Tennessee", "Indiana", "Missouri", "Maryland", "Wisconsin",
      "Colorado", "Minnesota", "South Carolina", "Alabama", "Louisiana"
    ),
    npi = c(
      "1234567890", "2345678901", "3456789012", "4567890123", "5678901234",
      "6789012345", "7890123456", "8901234567", "9012345678", "0123456789",
      "1357924680", "2468013579", "3691470258", "4815962037", "5927384610",
      "6048271359", "7159382604", "8260493715", "9371504826", "0482615937",
      "1593726048", "2604837159", "3715948260", "4826059371", "5937160482"
    ),
    for_redcap = c(rep("Yes", 20), rep("No", 5)),
    insurance_type = sample(c("Medicaid", "Private", "Self-Pay"), 25, replace = TRUE),
    appointment_requested = sample(c("New Patient", "Follow-up", "Urgent"), 25, replace = TRUE),
    caller_age = sample(18:45, 25, replace = TRUE),
    specialty_needed = sample(c("General OBGYN", "High-Risk", "Fertility", "Gynecologic Oncology"), 25, replace = TRUE),
    stringsAsFactors = FALSE
  )
}
create_realistic_census_data <- function() {
  # Simulate census data for testing
  data.frame(
    NAME = c(
      "Los Angeles County, California",
      "Cook County, Illinois",
      "Harris County, Texas",
      "Maricopa County, Arizona",
      "San Diego County, California"
    ),
    state = c("06", "17", "48", "04", "06"),
    county = c("037", "031", "201", "013", "073"),
    B01001_001E = c(10014009, 5275541, 4731145, 4485414, 3343364),  # Total population
    B01001_002E = c(4974799, 2573005, 2386199, 2281960, 1675975),   # Male
    B01001_026E = c(5039210, 2702536, 2344946, 2203454, 1667389),   # Female
    B01001_030E = c(189234, 89234, 78123, 67890, 56789),           # Female 15-17
    B01001_031E = c(145678, 67890, 56789, 45678, 34567),           # Female 18-19
    B01001_032E = c(167890, 78901, 67890, 56789, 45678),           # Female 20
    B01001_033E = c(178901, 89012, 78901, 67890, 56789),           # Female 21
    B01001_034E = c(345678, 167890, 145678, 123456, 98765),        # Female 22-24
    B01001_035E = c(456789, 234567, 201234, 178901, 145678),       # Female 25-29
    B01001_036E = c(467890, 245678, 212345, 189012, 156789),       # Female 30-34
    B01001_037E = c(445678, 223456, 198765, 175432, 142109),       # Female 35-39
    B01001_038E = c(423456, 212345, 189012, 165789, 132456),       # Female 40-44
    stringsAsFactors = FALSE
  )
}

# test -------------------------------------------------------------------------
skip_on_cran()
temp_dir <- tempdir()
error_scenarios <- list(
    empty_data = data.frame(),
    missing_columns = data.frame(id = 1:3, wrong_col = letters[1:3]),
    all_na_data = data.frame(
      id = 1:3, names = rep(NA, 3), practice_name = rep(NA, 3),
      phone_number = rep(NA, 3), state_name = rep(NA, 3),
      npi = rep(NA, 3), for_redcap = rep(NA, 3)
    )
  )
expect_error(
    mysterycall_clean_phase1(error_scenarios$empty_data, output_directory = temp_dir),
    "at least one row"
  )
expect_error(
    mysterycall_clean_phase1(error_scenarios$missing_columns, output_directory = temp_dir),
    "names"
  )
expect_error(
    mysterycall_clean_phase1(error_scenarios$all_na_data, output_directory = temp_dir),
    "names"  # Should fail validation due to missing required data
  )
