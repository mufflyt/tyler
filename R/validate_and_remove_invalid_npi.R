#' Validate and Remove Invalid NPI Numbers
#'
#' This function reads a CSV file containing NPI numbers, validates their
#' format using the npi package, and removes rows with missing or invalid NPIs.
#'
#' @param input_data Either a dataframe containing NPI numbers or a path to a CSV file.
#'
#' @return A dataframe containing valid NPI numbers.
#'
#' @importFrom npi npi_is_valid
#' @importFrom readr read_csv
#' @importFrom dplyr filter mutate
#'
#' @examples
#' # Example usage:
#' # input_data <- "~/path/to/your/NPI/file.csv"
#' # valid_df <- validate_and_remove_invalid_npi(input_data)
#'
#' @export
validate_and_remove_invalid_npi <- function(input_data) {

  if (is.data.frame(input_data)) {
    # Input is a dataframe
    npi_df <- input_data
  } else if (is.character(input_data)) {
    # Input is a file path to a CSV
    npi_df <- readr::read_csv(input_data)
  } else {
    stop("Input must be a dataframe or a file path to a CSV.")
  }

  # Remove rows with missing or empty NPIs
  npi_df <- npi_df %>%
    dplyr::filter(!is.na(npi) & npi != "")

  # If no rows remain after filtering, return empty dataframe
  if (nrow(npi_df) == 0) {
    return(npi_df)
  }

  # Add a new column "npi_is_valid" to indicate NPI validity
  npi_df <- npi_df %>%
    dplyr::mutate(npi_is_valid = as.logical(sapply(npi, function(x) {
      # Convert to character for consistent checking
      x_char <- as.character(x)
      # Check if it's exactly 10 characters and all digits
      if (nchar(x_char) == 10 && grepl("^[0-9]+$", x_char)) {
        npi::npi_is_valid(x_char)
      } else {
        FALSE
      }
      }))) %>%
    dplyr::filter(!is.na(npi_is_valid) & npi_is_valid)

  # Return the valid dataframe with the "npi_is_valid" column
  return(npi_df)
}

# Example usage:
# input_data <- "~/path/to/your/NPI/file.csv"
# valid_df <- validate_and_remove_invalid_npi(input_data)
