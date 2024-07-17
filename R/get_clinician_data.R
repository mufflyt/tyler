#' Validate and Remove Invalid NPI Numbers
#'
#' This function reads a CSV file containing NPI numbers, validates their
#' format using the npi package, and removes rows with missing or invalid NPIs.
#'
#' @param input_data Either a dataframe containing NPI numbers or a path to a CSV file.
#'
#' @return A dataframe containing valid NPI numbers.
#' @importFrom npi npi_is_valid
#' @importFrom readr read_csv
#' @importFrom dplyr filter mutate
#' @export
#'
validate_and_remove_invalid_npi <- function(input_data) {

  if (is.data.frame(input_data)) {
    # Input is a dataframe
    df <- input_data
  } else if (is.character(input_data)) {
    # Input is a file path to a CSV
    df <- readr::read_csv(input_data)
  } else {
    stop("Input must be a dataframe or a file path to a CSV.")
  }

  # Remove rows with missing or empty NPIs
  df <- df %>%
    dplyr::filter(!is.na(npi) & npi != "")

  # Add a new column "npi_is_valid" to indicate NPI validity
  df <- df %>%
    dplyr::mutate(npi_is_valid = sapply(npi, function(x) {
      if (is.numeric(x) && nchar(x) == 10) {
        npi::npi_is_valid(as.character(x))
      } else {
        FALSE
      }
    })) %>%
    dplyr::filter(!is.na(npi_is_valid) & npi_is_valid)

  # Return the valid dataframe with the "npi_is_valid" column
  return(df)
}

#' Retrieve Clinician Data
#'
#' This function retrieves clinician data for each valid NPI in the input dataframe.
#'
#' @param input_data Either a dataframe containing NPI numbers or a path to a CSV file.
#'
#' @return A tibble with clinician data for the provided NPIs.
#' @importFrom purrr map
#' @importFrom readr read_csv
#' @importFrom tidyr unnest_wider
#' @importFrom provider clinicians
#' @importFrom dplyr mutate
#'
#' @export
retrieve_clinician_data <- function(input_data) {

  if (is.data.frame(input_data)) {
    # Input is a dataframe
    df <- input_data
  } else if (is.character(input_data)) {
    # Input is a file path to a CSV
    df <- readr::read_csv(input_data)
  } else {
    stop("Input must be a dataframe or a file path to a CSV.")
  }

  # Function to retrieve clinician data for a single NPI
  get_clinician_data <- function(npi) {
    if (!is.numeric(npi) || nchar(npi) != 10) {
      cat("Invalid NPI:", npi, "\n")
      return(NULL)  # Skip this NPI
    }

    clinician_info <- provider::clinicians(npi = npi)
    if (is.null(clinician_info)) {
      cat("No results for NPI:", npi, "\n")
    } else {
      return(clinician_info)  # Return the clinician data
    }
    sleep(1)  # Sleep for a while to prevent overloading the server
  }

  # Clean the NPI numbers and retrieve clinician data
  df_updated <- input_data %>%
    validate_and_remove_invalid_npi() %>%
    dplyr::mutate(clinician_data = purrr::map(npi, get_clinician_data)) %>%
    tidyr::unnest_wider(clinician_data)

  return(df_updated)
}

# Use case
# validate_and_remove_invalid_npi <- validate_and_remove_invalid_npi
# retrieve_clinician_data <- retrieve_clinician_data
#
