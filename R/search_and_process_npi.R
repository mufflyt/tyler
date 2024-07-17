#' Search and Process NPI Numbers
#'
#' This function takes an input data frame containing first and last names, performs NPI search and processing,
#' and filters results based on specified taxonomies. It supports customization for enumeration type, search limit,
#' and filtering credentials.
#'
#' @param data A data frame with columns 'first' and 'last' containing the names to search.
#' @param enumeration_type The enumeration type for NPI search (e.g., "ind", "org", "all"). Default is "ind".
#' @param limit The maximum number of search results to request for each name pair. Default is 5.
#' @param country_code Filter for only the "US".
#' @param filter_credentials A character vector containing the credentials to filter the NPI results. Default is c("MD", "DO").
#' @param save_chunk_size The number of results to save per chunk. Default is 10.
#' @param dest_dir Destination directory to save chunked results. Default is NULL (current working directory).
#' @return A data frame containing the processed NPI search results.
#'
#' @import dplyr
#' @importFrom npi npi_search npi_flatten
#' @importFrom progress progress_bar
#' @importFrom purrr map2
#' @importFrom data.table rbindlist
#' @importFrom readr write_csv
#' @importFrom readxl read_xlsx
#' @importFrom data.table rbindlist
#' @export
search_and_process_npi <- memoise(function(data,
                                                    enumeration_type = "ind",
                                                    limit = 5L,
                                                    country_code = "US",
                                                    filter_credentials = c("MD", "DO"),
                                                    save_chunk_size = 10,
                                                    dest_dir = NULL) {

  cat("Starting search_and_process_npi...\n")

  # Check if 'data' is a data frame
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }
  cat("Input data is a data frame.\n")

  # Extract first and last names from the data frame
  first_names <- data$first
  last_names <- data$last

  # Define taxonomies to filter
  vc <- c("Allergy & Immunology", "Anesthesiology", "Dermatology", "Emergency Medicine", "Family Medicine", "Internal Medicine", "Obstetrics & Gynecology", "Ophthalmology", "Orthopaedic Surgery", "Pediatrics", "Psychiatry & Neurology", "Radiology", "Surgery", "Urology")
  bc <- c("Pathology", "Pediatrics", "Physical Medicine & Rehabilitation", "Plastic Surgery", "Preventive Medicine")

  # Function to search NPI based on first and last names
  search_npi <- function(first_name, last_name) {
    cat("Searching NPI for:", first_name, last_name, "\n")
    tryCatch(
      {
        # NPI search object
        npi_obj <- npi::npi_search(first_name = first_name, last_name = last_name)

        # Retrieve basic and taxonomy data from npi objects
        t <- npi::npi_flatten(npi_obj, cols = c("basic", "taxonomies"))

        # Subset results with taxonomy that matches taxonomies in the lists
        t <- t %>% dplyr::filter(taxonomies_desc %in% vc | taxonomies_desc %in% bc)
      },
      error = function(e) {
        cat("ERROR:", conditionMessage(e), "\n")
        return(NULL)  # Return NULL for error cases
      }
    )
    return(t)
  }

  # Create an empty list to receive the data
  out <- list()

  # Initialize progress bar
  total_names <- nrow(data)
  pb <- progress::progress_bar$new(total = total_names)

  # Function to save results to file
  save_results <- function(result, file_prefix, directory) {
    timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
    file_name <- file.path(directory, paste0(file_prefix, "_chunk_", timestamp, ".csv"))
    readr::write_csv(result, file_name, row.names = FALSE)
    cat("Saved chunk results to:", file_name, "\n")
  }

  # Initialize counters for chunk saving
  chunk_count <- 0

  # Search NPI for each name in the input data
  out <- purrr::map2(first_names, last_names, function(first_name, last_name) {
    pb$tick()
    result <- search_npi(first_name, last_name)

    # Increment chunk count and save results if save_chunk_size is reached
    if (length(result) > 0) {
      if (length(result) > save_chunk_size) {
        chunk_count <<- chunk_count + 1
        save_results(result, "results_of_search_and_process_npi", dest_dir)
      }
    }

    return(result)
  })

  # Filter npi_data to keep only elements that are data frames
  npi_data <- purrr::Filter(is.data.frame, out)

  # Combine multiple data frames into a single data frame
  result <- data.table::rbindlist(npi_data, fill = TRUE)

  # Return the result data frame
  return(result)
})

# Example usage:
# input_file <- "data-raw/acog_presidents.csv"
# output_result <- search_and_process_npi(input_file)
# readr::write_csv(output_result, "results_of_search_and_process_npi.csv")
