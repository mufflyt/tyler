#' Validate and Remove Invalid NPI Numbers
#'
#' This function reads a CSV file containing NPI numbers, validates their
#' format using the npi package, and removes rows with missing or invalid NPIs.
#'
#' @param input_csv_path Path to the CSV file containing NPI numbers.
#'
#' @return A dataframe containing valid NPI numbers.
#'
#' @examples
#' input_csv_path <- "~/Dropbox (Personal)/workforce/subspecialists_only.csv"
#' valid_df <- validate_and_remove_invalid_npi(input_csv_path)
#'
#' @export
validate_and_remove_invalid_npi <- function(input_csv_path) {
  library(dplyr)
  library(readr)
  library(npi)  # Load the npi package

  # Read the CSV file into a dataframe
  df <- read_csv(input_csv_path)

  # Remove rows with missing or empty NPIs
  df <- df %>%
    filter(!is.na(npi) & npi != "")

  # Add a new column "npi_is_valid" to indicate NPI validity
  df <- df %>%
    mutate(npi_is_valid = sapply(npi, function(x) {
      if (is.numeric(x) && nchar(x) == 10) {
        npi::npi_is_valid(as.character(x))
      } else {
        FALSE
      }
    })) %>%
    filter(!is.na(npi_is_valid) & npi_is_valid)

  # Return the valid dataframe with the "npi_is_valid" column
  return(df)
  readr::write_csv(valid_df, "data/subspecialists_valid_npi.csv")
}
# Example usage:
#input_csv_path <- "~/Dropbox (Personal)/workforce/subspecialists_only.csv"  # Replace with the path to your CSV file
#valid_df <- validate_and_remove_invalid_npi(input_csv_path)
