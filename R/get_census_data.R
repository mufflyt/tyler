#' Get Census data for all states' block groups
#'
#' This function retrieves Census data for all states' block groups by looping
#' over the specified list of state FIPS codes.
#'
#' @param us_fips A vector of state FIPS codes for which Census data is to be retrieved.
#' @return A dataframe containing Census data for all states' block groups.
#'
#' @import tigris
#' @import dplyr
#' @import censusapi
#' @importFrom tidyr
#'
#' @examples
#' us_fips_list <- tigris::fips_codes %>%
#' dplyr::select(state_code, state_name) %>%
#' dplyr::distinct(state_code, .keep_all = TRUE) %>%
#' dplyr::filter(state_code < 56) %>%
#' dplyr::select(state_code) %>%
#' dplyr::pull()
#'
#' us_fips_list <- c("01", "02", "04", "05", "06", "08", "09", "10", "11", "12", "13", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "44", "45", "46", "47", "48", "49", "50", "51", "53", "54", "55")
#'
#' all_census_data <- get_census_data(us_fips_list)
#'
#' Get Census data for all states' block groups
#'
#' This function retrieves Census data for all states' block groups by looping
#' over the specified list of state FIPS codes.
#'
#' @param us_fips_list A vector of state FIPS codes.  For example Colorado == 08.
#' @param vintage The vintage year for Census data (default is 2019).
#'
#' @return A dataframe containing Census data for all states' block groups.
#'
#'
#' @export
# Function to retrieve Census data for all states' block groups

get_census_data <- function(us_fips_list) {
  library(censusapi)
  library(dplyr)
  library(readr)

  # Initialize an empty list to store state data
  state_data <- list()

  # Loop over states for block group data
  for (f in us_fips) {
    us_fips <- tyler::fips
    print(f)

    # Define the region for the current state
    stateget <- paste("state:", f, "&in=county:*&in=tract:*", sep="")

    # Get Census data for the current state and append it to state_data list
    state_data[[f]] <- getCensus(
      name = "acs/acs5",
      vintage = 2019,
      vars = c("NAME", paste0("B01001_0", c("01", 26, 33:49), "E")), #B01001_001E
      region = "block group:*",
      regionin = stateget,
      key = "485c6da8987af0b9829c25f899f2393b4bb1a4fb")
  }

  # Bind together all state data into a single dataframe
  acs_raw <- dplyr::bind_rows(state_data)

  Sys.sleep(1)
  return(acs_raw)
}
#
# # Usage:
# us_fips_list <- c("01", "02", "04", "05", "06", "08", "09", "10", "11", "12", "13", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "44", "45", "46", "47", "48", "49", "50", "51", "53", "54", "55")
# all_census_data <- get_census_data(us_fips_list = fips)
