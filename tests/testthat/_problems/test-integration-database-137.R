# Extracted from test-integration-database.R:137

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
test_census_db <- create_test_census_db()
mock_get_census <- function(name, vintage, key, vars, region, ...) {
    if (region == "county:*") {
      # Return all counties
      result <- data.frame()
      for (state_code in names(test_census_db)) {
        for (county_code in names(test_census_db[[state_code]])) {
          county_data <- test_census_db[[state_code]][[county_code]]
          county_data$state <- state_code
          county_data$county <- county_code
          result <- rbind(result, county_data)
        }
      }
      return(result)
    }
    data.frame()
  }
with_mocked_bindings(
    getCensus = mock_get_census,
    .package = "censusapi",
    {
      # Test census data pipeline
      result <- mysterycall_get_census_data(
        geography = "county",
        state = "all",
        vintage = 2021,
        survey = "acs5"
      )

      expect_s3_class(result, "data.frame")
      expect_gt(nrow(result), 0)

      # Test summarization
      summary_result <- mysterycall_summarize_census(result)
      expect_s3_class(summary_result, "data.frame")
      expect_true("total_population" %in% names(summary_result) ||
                  "B01001_001E" %in% names(summary_result))
    }
  )
