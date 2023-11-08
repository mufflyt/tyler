#' Get Census data for all states' block groups
#'
#' This function retrieves Census data for all states' block groups by looping
#' over the specified list of state FIPS codes.
#'
#' @param us_fips A vector of state FIPS codes.
#' @param census_key The API key for the getCensus function.
#' @param vintage The vintage year for Census data (default is 2019).
#' @return A dataframe containing Census data for all states' block groups.
#'
#' @import tigris
#' @import dplyr
#' @import censusapi
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Usage:
#' # all_census_data <- get_census_data(us_fips_list, "your_census_api_key_here")
#' }
#'
#' #' Get Census data for all states' block groups
#'
#' This function retrieves Census data for all states' block groups by looping
#' over the specified list of state FIPS codes.
#'
#' @param us_fips A vector of state FIPS codes.
#' @param census_key The API key for the Census API.
#' @param vintage The vintage year for Census data (default is 2019).
#'
#' @return A dataframe containing Census data for all states' block groups.
#'
#' @import censusapi
#' @import dplyr
#' @import tigris
#'
#' @export
get_census_data <- function(us_fips, census_key, vintage = 2019) {
  # Check if the API key is provided
  if (missing(census_key) || is.null(census_key)) {
    stop("Please provide a valid Census API key.")
  }

  # Display setup instructions
  cat("\033[34mInstructions:\033[0m\n")
  cat("\033[34mall_census_data <- get_census_data(us_fips_list, 'your_census_api_key_here'):\033[0m\n")

  message("Getting Census data for all states' block groups...")

  # Initialize an empty list to store state data
  state_data <- list()

  # Loop over states for block group data
  for (f in us_fips) {
    message(paste("Fetching data for state FIPS code", f, "..."))

    # Define the region for the current state
    stateget <- paste("state:", f, "&in=county:*&in=tract:*", sep="")

    # Get Census data for the current state and append it to state_data list
    state_data[[f]] <- censusapi::getCensus(
      name = paste0("acs/acs", vintage),
      vintage = vintage,
      vars = c("NAME", paste0("B01001_0", c("01", 26, 33:49), "E")), #B01001_001E
      region = "block group:*",
      regionin = stateget,
      key = census_key)

    message(paste("Finished fetching data for state FIPS code", f))
  }

  # Bind together all state data into a single dataframe
  acs_raw <- dplyr::bind_rows(state_data)

  message("Census data retrieval completed.")

  return(acs_raw)
}

# Usage:
# all_census_data <- get_census_data(us_fips_list, "your_census_api_key_here")


# us_fips_list <- tigris::fips_codes %>%
#   dplyr::select(state_code, state_name) %>%
#   dplyr::distinct(state_code, .keep_all = TRUE) %>%
#   filter(state_code < 56) %>%
#   dplyr::select(state_code) %>%
#   pull()
#
# get_census_data <- function(us_fips, census_key, vintage = 2019) {
#   # Display setup instructions
#   cat("\033[34mInstructions:\033[0m\n")
#   cat("\033[34mall_census_data <- get_census_data(us_fips_list, `your_census_api_key_here`):\033[0m\n")
#
#   message("Getting Census data for all states' block groups...")
#
#   # Initialize an empty list to store state data
#   state_data <- list()
#
#   # Loop over states for block group data
#   for (f in us_fips) {
#     message(paste("Fetching data for state FIPS code", f, "..."))
#
#     # Define the region for the current state
#     stateget <- paste("state:", f, "&in=county:*&in=tract:*", sep="")
#
#     # Get Census data for the current state and append it to state_data list
#     state_data[[f]] <- censusapi::getCensus(
#       name = paste0("acs/acs", vintage),
#       vintage = vintage,
#       vars = c("NAME", paste0("B01001_0", c("01", 26, 33:49), "E")), #B01001_001E
#       region = "block group:*",
#       regionin = stateget,
#       key = census_key)
#
#     message(paste("Finished fetching data for state FIPS code", f))
#   }
#
#   # Bind together all state data into a single dataframe
#   acs_raw <- dplyr::bind_rows(state_data)
#
#   message("Census data retrieval completed.")
#
#   return(acs_raw)
# }
#
# # Usage:
# # all_census_data <- get_census_data(us_fips_list, "your_census_api_key_here")
