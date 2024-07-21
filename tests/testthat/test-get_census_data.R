library(testthat)
library(dplyr)
library(mockery)
library(censusapi)

# Define the get_census_data function for the testing environment
get_census_data <- function(us_fips_list, vintage = 2022) {
  library(dplyr)
  library(censusapi)

  # Initialize an empty list to store state data
  state_data <- list()

  # Loop over states for block group data
  for (f in us_fips_list) {
    cat("Processing FIPS:", f, "\n")

    # Define the region for the current state
    stateget <- paste("state:", f, "&in=county:*&in=tract:*", sep = "")

    # Get Census data for the current state and append it to state_data list
    state_data[[f]] <- censusapi::getCensus(
      name = "acs/acs5",
      vintage = vintage,
      vars = c("NAME", paste0("B01001_0", c("01", 26, 33:49), "E")), # B01001_001E
      region = "block group:*",
      regionin = stateget,
      key = "485c6da8987af0b9829c25f899f2393b4bb1a4fb"
    )
  }

  # Bind together all state data into a single dataframe
  acs_raw <- dplyr::bind_rows(state_data)

  Sys.sleep(1)
  return(acs_raw)
}

# Mock function to simulate censusapi::getCensus
mock_getCensus <- function(name, vintage, vars, region, regionin, key) {
  cat("Mock getCensus called with:", regionin, "\n")
  return(data.frame(
    NAME = paste("Block Group", regionin),
    B01001_001E = sample(100:1000, 1),
    B01001_026E = sample(100:1000, 1),
    B01001_033E = sample(100:1000, 1),
    B01001_034E = sample(100:1000, 1),
    B01001_035E = sample(100:1000, 1),
    B01001_036E = sample(100:1000, 1),
    B01001_037E = sample(100:1000, 1),
    B01001_038E = sample(100:1000, 1),
    B01001_039E = sample(100:1000, 1),
    B01001_040E = sample(100:1000, 1),
    B01001_041E = sample(100:1000, 1),
    B01001_042E = sample(100:1000, 1),
    B01001_043E = sample(100:1000, 1),
    B01001_044E = sample(100:1000, 1),
    B01001_045E = sample(100:1000, 1),
    B01001_046E = sample(100:1000, 1),
    B01001_047E = sample(100:1000, 1),
    B01001_048E = sample(100:1000, 1),
    B01001_049E = sample(100:1000, 1)
  ))
}

# Test cases
test_that("Retrieves Census data for valid FIPS codes", {
  cat("Running test: Retrieves Census data for valid FIPS codes\n")
  valid_fips <- c("01", "02")

  # Stub the getCensus function
  stub(get_census_data, 'censusapi::getCensus', mock_getCensus)

  result <- get_census_data(valid_fips)

  expect_true(nrow(result) > 0)
  expect_true("NAME" %in% colnames(result))
  expect_true("B01001_001E" %in% colnames(result))
})

test_that("Handles empty FIPS list gracefully", {
  cat("Running test: Handles empty FIPS list gracefully\n")
  empty_fips <- c()

  result <- get_census_data(empty_fips)

  expect_equal(nrow(result), 0)
})

test_that("Handles invalid FIPS codes gracefully", {
  cat("Running test: Handles invalid FIPS codes gracefully\n")
  invalid_fips <- c("ZZ", "XX")

  # Stub the getCensus function to return NULL for invalid FIPS
  stub(get_census_data, 'censusapi::getCensus', function(...) NULL)

  result <- get_census_data(invalid_fips)

  expect_equal(nrow(result), 0)
})
