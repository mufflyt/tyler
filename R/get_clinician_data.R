#' Retrieve Clinician Data for NPIs
#'
#' This function reads a CSV file containing National Provider Identifiers (NPIs) or a dataframe,
#' retrieves clinician data for each valid NPI using the provider package.
#' https://cran.r-project.org/web/packages/devtools/vignettes/dependencies.html
#' https://r-pkgs.org/dependencies-in-practice.html#depending-on-the-development-version-of-a-package
#'
#'
#' @param input_csv_path Path to the input CSV file containing NPIs.
#'
#' @return A tibble with clinician data for the provided NPIs.
#'
#' @import dplyr
#' @import purrr
#' @import readr
#' @import tidyr
#' @import lubridate
#' @import memoise
#' @import zipcodeR
#' @import npi
#'
#' @export

# fc <- cache_filesystem(file.path(".cache"))
# gc()

#Function 1: validate_and_remove_invalid_npi
validate_and_remove_invalid_npi <- function(input_data) {
  library(dplyr)
  library(npi)  # Load the npi package
  library(readr)

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
    #head(5) %>%. #for testing only
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



#####################
#Function 2: retrieve_clinician_data
## Output
df_updated <- NULL

retrieve_clinician_data <- function(input_data) {
  library(provider)
  library(dplyr)
  library(purrr)
  library(readr)
  library(tidyr)
  library(lubridate)
  library(memoise)
  library(zipcodeR)

  # Load libraries
  #remotes::install_github("andrewallenbruce/provider")

  if (is.data.frame(input_data)) {
    # Input is a dataframe
    df <- input_data
  } else if (is.character(input_data)) {
    # Input is a file path to a CSV
    df <- readr::read_csv(input_data)
  } else {
    stop("Input must be a dataframe or a file path to a CSV.")
  }

  # Clean the NPI numbers
  df <- validate_and_remove_invalid_npi(df)

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
      return(clinician_info)  # Print the clinician data as it comes out
    }
    Sys.sleep(1)
  }

  #df <- df %>% head(5) #test

  # Loop through the "npi" column and get clinician data
  df_updated <- df %>%
    dplyr::mutate(row_number = row_number()) %>%
    dplyr::mutate(clinician_data = purrr::map(npi, get_clinician_data)) %>%
    tidyr::unnest(clinician_data, names_sep = "_") %>%
    dplyr::distinct(npi, .keep_all = TRUE)

  return(df_updated)
}
# #Use Case
# #
# # Call the retrieve_clinician_data function with an NPI value
# input_data <- ("subspecialists_only.csv")
# clinician_data <- retrieve_clinician_data(input_data)
