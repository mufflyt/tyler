#' Get Census data of all state block groups
#'
#' This function retrieves Census data of all state block groups by looping
#' over the specified list of state FIPS codes.
#'
#' @param us_fips_list A vector of state FIPS codes which Census data is to be retrieved.
#' @param vintage The vintage year is Census data (default is 2022).
#'
#' @return A dataframe containing Census data all state block groups.
#' @importFrom dplyr bind_rows
#' @importFrom censusapi getCensus
#' @export
get_census_data <- function(us_fips_list, vintage = 2022) {

  library(dplyr)
  library(censusapi)

  # Initialize an empty list to store state data
  state_data <- list()

  # Loop over states for block group data
  for (f in us_fips_list) {
    cat("Processing FIPS:", f, "\n")

    # Define the region for the current state
    stateget <- paste("state:", f, "&in=county:*&in=tract:*", sep="")

    # Get Census data for the current state and append it to state_data list
    state_data[[f]] <- censusapi::getCensus(
      name = "acs/acs5",
      vintage = vintage,
      vars = c("NAME", paste0("B01001_0", c("01", 26, 33:49), "E")), #B01001_001E
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

# Usage example:
# us_fips_list <- c("01", "02", "04", "05", "06", "08", "09", "10", "11", "12",
#                   "13", "15", "16", "17", "18", "19", "20", "21", "22", "23",
#                   "24", "25", "26", "27", "28", "29", "30", "31", "32", "33",
#                   "34", "35", "36", "37", "38", "39", "40", "41", "42", "44",
#                   "45", "46", "47", "48", "49", "50", "51", "53", "54", "55")
# all_census_data <- get_census_data(us_fips_list)
