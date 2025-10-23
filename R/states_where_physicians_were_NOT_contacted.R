#' Summarize States Where Physicians Were NOT Contacted
#'
#' This function summarizes the demographic details by identifying the states where physicians
#' were not successfully contacted and those that were included. It also reports how many unique
#' physicians were successfully reached by filtering to rows with affirmative contact indicators
#' (e.g., "Yes" values in `contact_office` or `included_in_study`).
#'
#' @param filtered_data A data frame containing filtered data of contacted physicians.
#' @param all_states A character vector of all possible states including Washington, DC.
#' If not provided, a default set of states will be used.
#'
#' @return A character string summarizing the inclusion and exclusion of states
#'   alongside the count of unique physicians successfully contacted.
#' @family summary
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

  # Helper to coerce character responses such as "Yes"/"No" into logical
  as_positive_logical <- function(x) {
    if (is.logical(x)) {
      return(!is.na(x) & x)
    }
    if (is.numeric(x)) {
      return(!is.na(x) & x != 0)
    }
    normalised <- tolower(trimws(as.character(x)))
    !is.na(normalised) & normalised %in% c("yes", "y", "true", "1")
  }

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

  # Limit the data to rows that represent completed contacts
  contacted_mask <- rep(TRUE, nrow(filtered_data))
  if ("contact_office" %in% names(filtered_data)) {
    contacted_mask <- contacted_mask & as_positive_logical(filtered_data$contact_office)
  }
  if ("included_in_study" %in% names(filtered_data)) {
    contacted_mask <- contacted_mask & as_positive_logical(filtered_data$included_in_study)
  }
  contacted_data <- filtered_data[contacted_mask, , drop = FALSE]

  # Identify the unique included states
  if ("state" %in% names(contacted_data)) {
    included_states <- dplyr::distinct(contacted_data, state)
  } else if ("state" %in% names(filtered_data)) {
    included_states <- dplyr::distinct(filtered_data[, "state", drop = FALSE])
  } else {
    included_states <- dplyr::distinct(data.frame(state = character()))
  }

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

  # Determine how many unique physicians were successfully contacted
  unique_physicians <- 0L
  identifier_columns <- intersect(
    c("npi", "name", "physician_info", "physician_information"),
    names(contacted_data)
  )
  if (length(identifier_columns) > 0) {
    id_values <- contacted_data[[identifier_columns[[1]]]]
    id_values <- as.character(id_values)
    id_values <- id_values[!is.na(id_values) & nzchar(id_values)]
    unique_physicians <- length(unique(id_values))
  } else if (nrow(contacted_data)) {
    unique_physicians <- nrow(dplyr::distinct(contacted_data))
  }

  # Prepare the output string
  output_string <- paste0(
    "A total of ",
    unique_physicians,
    " unique physicians were identified in the dataset and were successfully contacted (i.e., with a recorded wait time for an appointment) in ",
    num_included_states,
    " states including the District of Columbia. The excluded states include ",
    excluded_states_series,
    "."
  )

  beepr::beep(2)
  return(output_string)
}



# Example usage:
# filtered_data <- data.frame(state = c("California", "New York", "Texas"))
# states_where_physicians_were_NOT_contacted(filtered_data)
