#' Search and Process NPI Numbers
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
#' @param country_code Filter for only the "US".
#' @param filter_credentials A character vector containing the credentials to filter the
#'   NPI results. Default is c("MD", "DO").
#' @return A list containing the processed result data frame and the percentage of "NO MATCH FOUND".
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
#' @import dplyr
#' @import memoise
#' @import readr
#' @import readxl
#' @import npi
#'
#' @export
search_and_process_npi <- function(input_file,
                                   enumeration_type = "ind",
                                   limit = 5L,
                                   country_code = "US",
                                   filter_credentials = c("MD", "DO")) {

  # For testing:
  # input_file <- "/Users/tylermuffly/Dropbox (Personal)/Nomogram/nomogram/data/nppes_search/Lo_R_Author.csv"
  # enumeration_type <- "ind"
  # limit<-5L
  # country_code<- "US"
  # filter_credentials <- c("MD", "DO")

  if (!file.exists(input_file)) {
    stop(
      "The specified file '", input_file, "' does not exist.\n",
      "Please provide the full path to the file."
    )
  }

  file_extension <- tools::file_ext(input_file)

  if (file_extension == "rds") {
    data <- readr::read_rds(input_file)
  } else if (file_extension %in% c("csv", "xls", "xlsx")) {
    if (file_extension %in% c("xls", "xlsx")) {
      data <- readxl::read_xlsx(input_file)
    } else {
      data <- readr::read_csv(input_file)
    }
  } else {
    stop("Unsupported file format. Please provide an RDS, CSV, or XLS/XLSX file.")
  }

  #fc <- cache_filesystem(file.path(".cache"))
  #npi_search_memo <- memoise::memoise(npi_search, cache = fc)

  data <- data %>% tail(10)

  first_names <- data$first
  last_names <- data$last

  search_npi <- function(first_name, last_name) {
    tryCatch(
      {
        npi::npi_search(
          first_name = first_name,
          last_name = last_name,
          enumeration_type = enumeration_type,
          limit = limit
        )
      },
      error = function(e) {
        message(sprintf("Error for %s %s\n%s", first_name, last_name, e$message))
        NULL
      }
    )
  }

  out <- list()

  out <- purrr::map2(first_names, last_names, function(first_name, last_name) {
    search_npi(first_name, last_name)
  })

  npi_data <- purrr::map_dfr(out, npi::npi_flatten) %>%
    tidyr::unite(full_name, basic_first_name, basic_last_name, sep = " ", remove = FALSE, na.rm = FALSE) %>%
    dplyr::distinct(npi, .keep_all = TRUE) %>%
    dplyr::filter(!is.na(basic_credential))

  result <- npi_data

  date_time <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

  filename <- paste("results_of_search_and_process_npi", date_time, ".rds", sep = "_")

  readr::write_rds(result, file = filename)

  # Return the result data frame
  return(result)
}

#
# input_file <- "/Users/tylermuffly/Dropbox (Personal)/Nomogram/nomogram/data/nppes_search/Lo_R_Author.csv"
# output_result <- search_and_process_npi(input_file,
#                                         enumeration_type = "ind",
#                                         limit = 5L,
#                                         country_code = "US",
#                                         filter_credentials = c("MD", "DO"))
