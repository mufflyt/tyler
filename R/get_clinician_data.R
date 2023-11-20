#' #' Retrieve Clinician Data for NPIs
#' #'
#' #' This function reads a CSV file containing National Provider Identifiers (NPIs),
#' #' retrieves clinician data for each valid NPI using the provider package, and saves
#' #' the updated data as a CSV file with a timestamp in the filename.
#' #' https://cran.r-project.org/web/packages/devtools/vignettes/dependencies.html
#' #' https://r-pkgs.org/dependencies-in-practice.html#depending-on-the-development-version-of-a-package
#' #'
#' #'
#' #' @param input_csv_path Path to the input CSV file containing NPIs.
#' #'
#' #' @return A tibble with clinician data for the provided NPIs.
#' #'
#' #' @examples
#' #' \dontrun{
#' #'   # Define the path to the input CSV file
#' #'   input_csv_path <- "your_input.csv"
#' #'
#' #'   # Call the retrieve_clinician_data function with the input file
#' #'   result <- retrieve_clinician_data(input_csv_path)
#' #'
#' #'   # Further data analysis or processing steps can be performed on 'result'
#' #' }
#' #'
#' #' @import provider
#' #' @import dplyr
#' #' @import purrr
#' #' @import readr
#' #' @import tidyr
#' #' @import lubridate
#' #' @import memoise
#' #' @import zipcodeR
#' #'
#' #' @export
#' fc <- cache_filesystem(file.path(".cache"))
#' gc()
#'
#' ## Output
#' df_updated <- NULL
#'
#' retrieve_clinician_data <- function(input_csv_path) {
#'   #input_csv_path <- ("~/Dropbox (Personal)/workforce/subspecialists_only.csv") #for testing
#'   library(provider)
#'   library(dplyr)
#'   library(purrr)
#'   library(readr)
#'   library(tidyr)
#'   library(lubridate)
#'   library(memoise)
#'   library(zipcodeR)
#'
#'   # Load libraries
#'   remotes::install_github("andrewallenbruce/provider")
#'
#'   # Create a memoized version of the clinicians function
#'   memoized_clinicians <- memoise(provider::clinicians, cache = fc)
#'
#'   # Read the CSV file into a dataframe
#'   df <- readr::read_csv(input_csv_path) %>%
#'     dplyr::filter(!is.na(npi)) %>%
#'     dplyr::distinct(npi, .keep_all = TRUE) %>%
#'     dplyr::mutate(npi = as.numeric(npi))
#'
#'   #df <- df %>% head(10) # For testing
#'
#'   # Function to retrieve clinician data for a single NPI
#'   get_clinician_data <- function(npi) {
#'     if (!is.numeric(npi) || nchar(npi) != 10) {
#'       cat("Invalid NPI:", npi, "\n")
#'       return(NULL)  # Skip this NPI
#'     }
#'
#'     # Use the memoized function to retrieve clinician data
#'     clinician_info <- memoized_clinicians(npi = npi)
#'
#'     clinician_info <- provider::clinicians(npi = npi)
#'     if (is.null(clinician_info)) {
#'       cat("No results for NPI:", npi, "\n")
#'     } else {
#'       print(clinician_info)  # Print the clinician data as it comes out
#'     }
#'     #return(clinician_info)
#'     Sys.sleep(1)
#'   }
#'
#'   # Loop through the "npi" column and get clinician data
#'   df_updated <- df %>%
#'     dplyr::mutate(row_number = row_number()) %>%
#'     dplyr::mutate(clinician_data = purrr::map(npi, get_clinician_data)) %>%
#'     tidyr::unnest(clinician_data, names_sep = "_") %>%
#'     dplyr::distinct(npi, .keep_all = TRUE)
#'
#'   # Save the updated dataframe as a CSV with the current date and time in the filename
#'   timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
#'   output_csv <- paste("data/updated_data_", timestamp, ".csv", sep = "")
#'   readr::write_csv(df_updated, file = output_csv)
#'
#'   # Return the path to the saved CSV file
#'   cat("Updated data saved as:", output_csv, "\n")
#'
#'   return(df_updated)
#' }
#' #Use Case
#' #
#' # Call the retrieve_clinician_data function with an NPI value
#' # input_csv_path <- ("~/Dropbox (Personal)/workforce/subspecialists_only.csv")
#' # clinician_data <- retrieve_clinician_data(input_csv_path)
