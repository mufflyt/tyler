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

# Example usage:
# input_data <- "~/path/to/your/NPI/file.csv"
# valid_df <- validate_and_remove_invalid_npi(input_data)
