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
#' @family npi
#' @export
#'
validate_and_remove_invalid_npi <- function(input_data) {

  cat("Starting validate_and_remove_invalid_npi...\n")

  if (is.data.frame(input_data)) {
    cat("Input is a data frame.\n")
    clinician_df <- input_data
  } else if (is.character(input_data)) {
    cat("Input is a file path to a CSV.\n")
    clinician_df <- readr::read_csv(input_data, col_types = readr::cols(npi = readr::col_character()))
  } else {
    stop("Input must be a dataframe or a file path to a CSV.")
  }

  cat("Initial dataframe:\n")
  print(clinician_df)

  clinician_df <- clinician_df %>%
    dplyr::filter(!is.na(npi) & npi != "")

  cat("After filtering missing or empty NPIs:\n")
  print(clinician_df)

  clinician_df <- clinician_df %>%
    dplyr::mutate(npi_is_valid = sapply(npi, function(x) {
      if (nchar(x) == 10) {
        npi::npi_is_valid(x)
      } else {
        FALSE
      }
    })) %>%
    dplyr::filter(!is.na(npi_is_valid) & npi_is_valid)

  cat("After filtering invalid NPIs:\n")
  print(clinician_df)
  cat("validate_and_remove_invalid_npi completed.\n")

  return(clinician_df)
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
#' @family npi
#' @export
#' @examples
#' \dontrun{
#' clinician_df <- retrieve_clinician_data("clinicians.csv")
#' }
retrieve_clinician_data <- function(input_data) {
  if (!requireNamespace("provider", quietly = TRUE)) {
    stop(
      "Package 'provider' is required for this function. " ,
      "Install it from GitHub with remotes::install_github('andrewallenbruce/provider').",
      call. = FALSE
    )
  }

  if (is.data.frame(input_data)) {
    # Input is a dataframe
    clinician_df <- input_data
  } else if (is.character(input_data)) {
    # Input is a file path to a CSV
    clinician_df <- readr::read_csv(input_data)
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
  }

  # Clean the NPI numbers and retrieve clinician data
  df_updated <- validate_and_remove_invalid_npi(clinician_df) %>%
    dplyr::mutate(clinician_data = purrr::map(npi, get_clinician_data)) %>%
    tidyr::unnest_wider(clinician_data)

  beepr::beep(2)
  return(df_updated)
}
