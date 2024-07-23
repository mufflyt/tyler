#' Summarize States Where Physicians Were NOT Contacted
#'
#' This function summarizes the demographic details by identifying the states where physicians
#' were not successfully contacted and those that were included.
#'
#' @param filtered_data A data frame containing filtered data of contacted physicians.
#' @param all_states A character vector of all possible states including Washington, DC.
#' If not provided, a default set of states will be used.
#'
#' @return A character string summarizing the inclusion and exclusion of states.
#' @export
#'
#' @importFrom dplyr distinct
#' @importFrom stringr str_c
#'
#' @examples
#' # Example with provided all_states
#' filtered_data <- data.frame(state = c("California", "New York", "Texas"))
#' all_states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
#'                  "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho",
#'                  "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana",
#'                  "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
#'                  "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada",
#'                  "New Hampshire", "New Jersey", "New Mexico", "New York",
#'                  "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon",
#'                  "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota",
#'                  "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
#'                  "Washington", "West Virginia", "Wisconsin", "Wyoming",
#'                  "District of Columbia")
#' states_where_physicians_were_NOT_contacted(filtered_data, all_states)
#'
#' # Example with default all_states
#' filtered_data <- data.frame(state = c("California", "New York", "Texas", "Nevada"))
#' states_where_physicians_were_NOT_contacted(filtered_data)
#'
states_where_physicians_were_NOT_contacted <- function(filtered_data, all_states = NULL) {

  # Default list of all states including Washington, DC
  default_all_states <- c(
    "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",
    "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
    "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey",
    "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
    "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming", "District of Columbia"
  )

  # Check if all_states is NULL and use default if necessary
  if (is.null(all_states)) {
    all_states <- default_all_states
  }

  # Identify the unique included states
  included_states <- dplyr::distinct(filtered_data, state)

  # Identify the excluded states by finding the difference between all states and the included states
  excluded_states <- setdiff(all_states, included_states$state)

  # Ensure "District of Columbia" is not included in the excluded states if it is in the included states
  if ("District of Columbia" %in% included_states$state) {
    excluded_states <- setdiff(excluded_states, "District of Columbia")
  }

  # Convert the excluded_states vector into a human-readable series
  excluded_states_series <- if (length(excluded_states) > 1) {
    excluded_series <- paste(paste(excluded_states[-length(excluded_states)], collapse = ", "), "and", excluded_states[length(excluded_states)])
    excluded_series
  } else if (length(excluded_states) == 1) {
    excluded_states
  } else {
    "No states"
  }

  # Count the number of included states
  num_included_states <- length(included_states$state)

  # Prepare the output string
  output_string <- paste0(
    "A total of unique physicians were identified in the dataset and were successfully contacted (i.e., with a recorded wait time for an appointment) in ",
    num_included_states,
    " states including the District of Columbia. The excluded states include ",
    excluded_states_series,
    "."
  )

  return(output_string)
}



# Example usage:
# filtered_data <- data.frame(state = c("California", "New York", "Texas"))
# states_where_physicians_were_NOT_contacted(filtered_data)
