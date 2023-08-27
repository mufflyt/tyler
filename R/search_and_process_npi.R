#' A function to search and process NPI numbers.
#'
#' This function takes an input file in RDS, CSV, or XLS/XLSX format, reads the data,
#' performs NPI search and processing, and returns the results along with the percentage
#' of "NO MATCH FOUND". The function also provides customization options for the user
#' to specify the enumeration type, search limit, and filtering criteria for NPI results.
#'
#' @param input_file The path to the input file (RDS, CSV, or XLS/XLSX) containing the data.
#' @param enumeration_type The enumeration type for NPI search (e.g., "ind", "org", "all").
#'   Default is "ind".
#' @param limit The maximum number of search results to request for each name pair.
#'   Default is 10.
#' @param filter_credentials A character vector containing the credentials to filter the
#'   NPI results. Default is c("MD", "DO").
#' @param country_code Filter for only the "US".
#' @return A list containing the processed result data frame and the percentage of "NO MATCH FOUND".
#' @export
#'
#' @examples
#' \dontrun{
#' # Call the function with the input file name
#' input_file <- "data/Sent_to_npi_search.xlsx"
#' output <- search_and_process_npi(input_file)
#'
#' # Access the result and the percentage of "NO MATCH FOUND" from the function's output
#' result_df <- output$result
#' no_match_percentage <- output$no_match_percentage
#' no_match_percentage
#' }



# Create a function to search and process NPI numbers
search_and_process_npi <- function(input_file,
                                   enumeration_type = "ind",
                                   limit = 5L,
                                   country_code = "US",
                                   filter_credentials = c("MD", "DO")) {

  # Load necessary packages
  library(dplyr)
  library(stats)  # Import stats package to access na.omit
  library(devtools)
  install_github("ropensci/npi")
  library(npi)
  library(tidyverse)
  library(memoise)
  library(humaniformat)

  # Error handling of the humans
  # Check if the input_file contains the full path name.
  # If not, remind the user to use the full file path.
  if (!file.exists(input_file)) {
    stop("The specified file '", input_file, "' does not exist.\n",
         "Please provide the full path to the file.")
  }

  # Read data from different file formats (RDS, CSV, or XLS/XLSX)
  file_extension <- tools::file_ext(input_file)

  if (file_extension == "rds") {
    data <- readRDS(input_file)
  } else if (file_extension %in% c("csv", "xls", "xlsx")) {
    if (file_extension %in% c("xls", "xlsx")) {
      data <- readxl::read_xlsx(input_file)
    } else {
      data <- readr::read_csv(input_file)
    }
  } else {
    stop("Unsupported file format. Please provide an RDS, CSV, or XLS/XLSX file.")
  }

  # Define a memoised version of the npi_search function
  gc()
  fc <- cache_filesystem(file.path(".cache"))
  npi_search_memo <- memoise(npi_search, cache = fc)
#
#   # Read data from the input file
#   data <- readxl::read_xlsx(input_file) %>%
#     dplyr::mutate(first = humaniformat::first_name(full_name),
#                   last = humaniformat::last_name(full_name),
#                   last = stringr::str_remove_all(last, "[^[:alnum:]]")) %>%
#     stats::na.omit()  # Remove rows with missing first or last names

  # Define input data from df
  first_names <- data$first
  last_names <- data$last

  # Create a function to perform the NPI search and processing
  search_npi <- function(first_name, last_name) {
    tryCatch({
      npi_search_memo(first_name = first_name,
                      last_name = last_name,
                      enumeration_type = "ind",
                      limit = 10)
    }, error = function(e) {
      message(sprintf("Error for %s %s\n%s", first_name, last_name, e$message))
      NULL
    })
  }

  # Create an empty list to store the search results
  out <- list()

  # Perform the search for each name pair using purrr::map2 function
  purrr::walk2(first_names, last_names, function(first_name, last_name) {
    print(paste("Searching for:", first_name, last_name))
    out[[length(out) + 1]] <- search_npi(first_name, last_name)
  })

  # Progress bar
  total_names <- length(first_names)
  processed_names <- 0

  # Perform the search for each name pair using purrr::map function
  out <- purrr::map2(first_names, last_names, function(first_name, last_name) {
    processed_names <- processed_names + 1
    cat(sprintf("Processing name %d of %d: %s %s\n", processed_names, total_names, first_name, last_name))
    out[[length(out) + 1]] <- search_npi(first_name, last_name)
  })

  # Perform the search for each name pair using purrr::map function
  out <- purrr::map2(first_names, last_names, search_npi)

  # Combine search results into a single data frame
  npi_data <- purrr::map_dfr(out, npi::npi_flatten) %>%
    dplyr::distinct(npi, .keep_all = TRUE) %>%
    dplyr::filter(addresses_country_name == "United States") %>%
    dplyr::mutate(basic_credential = stringr::str_remove_all(basic_credential, "[[\\p{P}][\\p{S}]]")) %>%
    dplyr::filter(stringr::str_to_lower(basic_credential) %in% stringr::str_to_lower(c("MD", "DO"))) %>%
    dplyr::arrange(basic_last_name)

  # Create a helper function to concatenate first and last names for joining
  create_full_name <- function(first, last) {
    stringr::str_c(stringr::str_to_lower(first), stringr::str_to_lower(last))
  }

  # Join the data dataframe with the df dataframe, bring in the original data
  result <- dplyr::left_join(data, npi_data, by = "full_name", suffix = c("", "_npi"))

  # Replace missing NPI numbers with "NO MATCH FOUND"
  result$npi <- dplyr::if_else(is.na(result$npi), "NO MATCH FOUND", result$npi)

  # Calculate the percentage of "NO MATCH FOUND"
  no_match_percentage <- base::round((sum(result$npi == "NO MATCH FOUND") / nrow(result)) * 100, digits = 1)

  # Access the current date and time
  date_time <- base::format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

  # Create the filename with function name and date-time
  filename <- base::paste("search_and_process_npi", date_time, ".rds", sep = "_")

  # Save the result_df as an RDS file
  base::saveRDS(result, file = filename)

  # Return the result data frame and the percentage of "NO MATCH FOUND"
  return(list(result = result, no_match_percentage = no_match_percentage))
}

# Call the function with the input file name
# input_file <- "data/Sent_to_npi_search.xlsx"
# output <- search_and_process_npi(input_file)

