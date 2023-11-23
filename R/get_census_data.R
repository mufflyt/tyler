#' Get Census data for all states' block groups
#'
#' This function retrieves Census data for all states' block groups by looping
#' over the specified list of state FIPS codes.
#'
#' @param us_fips A vector of state FIPS codes.
#' @param vintage The vintage year for Census data (default is 2019).
#' @return A dataframe containing Census data for all states' block groups.
#'
#' @import tigris
#' @import dplyr
#' @import censusapi
#' @import censusapi
#' @import dplyr
#' @import tigris
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Usage:
#'   # all_census_data <- get_census_data(us_fips_list, "your_census_api_key_here")
#' }
#'
#' Get Census data for all states' block groups
#'
#' This function retrieves Census data for all states' block groups by looping
#' over the specified list of state FIPS codes.
#'
#' @param us_fips A vector of state FIPS codes.
#' @param vintage The vintage year for Census data (default is 2019).
#'
#' @return A dataframe containing Census data for all states' block groups.
#'
#'
#' @export
# Function to retrieve Census data for all states' block groups
get_census_data <- function(us_fips) {
  # Initialize an empty list to store state data
  state_data <- list()

  # Loop over states for block group data
  for (f in us_fips) {
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

  return(acs_raw)
}
#
# # Usage:
# # all_census_data <- get_census_data(us_fips_list, "your_census_api_key_here")
