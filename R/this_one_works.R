#' Scrape Physicians' Data with Tor
#'
#' This function scrapes data for physicians within a specified ID range, excluding wrong IDs.
#'
#' @param startID The starting ID for scraping.
#' @param endID The ending ID for scraping.
#' @param torPort The port number for Tor SOCKS proxy.
#' @param wrong_ids_path Optional path to a CSV of IDs to skip.
#' @param verbose Logical; if TRUE, prints status messages while running. Default is FALSE.
#' @return A dataframe containing scraped physicians' data.
#'
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows
#' @importFrom readr read_csv write_csv
#'
#' @examples
#' # Call the function
#' scrape_result <- scrape_physicians_data_with_tor(startID = 9045999, endID = 9046000, torPort = 9150)
#'
scrape_physicians_data_with_tor <- function(startID, endID, torPort, wrong_ids_path = NULL, verbose = FALSE) {
  if (isTRUE(verbose)) {
    message("Starting scrape_physicians_data_with_tor...")
    message("Parameters - startID: ", startID, " endID: ", endID, " torPort: ", torPort)
  }

  # Create a sequence of IDs from startID to endID
  id_list <- seq(startID, endID)
  if (isTRUE(verbose)) {
    message("ID list: ", paste(id_list, collapse = ", "))
  }

  # Load the list of wrong IDs to exclude from scraping
  wrongs1 <- if (!is.null(wrong_ids_path)) readr::read_csv(wrong_ids_path) else data.frame(WrongIDs = integer())
  wrong_ids <- wrongs1$WrongIDs
  if (isTRUE(verbose)) {
    message("Wrong IDs loaded: ", paste(wrong_ids, collapse = ", "))
  }

  # Initialize data frames to store physicians' data and wrong IDs
  Physicians <- data.frame()
  WrongIDs <- c()
  if (isTRUE(verbose)) {
    message("Initialized empty data frames for Physicians and WrongIDs.")
  }

  # Temporary directory for output files
  temp_dir <- tempdir()

  # API Inputs
  base <- "https://api.abog.org/"
  endpoint <- "diplomate/"
  action <- "/verify"

  for (id in id_list) {
    if (isTRUE(verbose)) {
      message("Processing ID: ", id)
    }

    # Check if the ID is in the list of wrong IDs
    if (id %in% wrong_ids) {
      if (isTRUE(verbose)) {
        message("Skipping wrong ID: ", id)
      }
      next
    }

    # Create the API URL for the current ID
    url <- paste(base, endpoint, id, action, sep = "")
    if (isTRUE(verbose)) {
      message("API URL: ", url)
    }

    # Send a GET request through Tor
    ph_r <- httr::GET(url, use_proxy(paste0("socks5://localhost:", torPort)))

    # Check if the request was successful
    if (ph_r$status_code == 200) {
      if (isTRUE(verbose)) {
        message("Request successful for ID: ", id)
      }

      # Parse JSON response
      ph_data <- jsonlite::fromJSON(httr::content(ph_r, as = "text"))

      # Check if the response is not empty
      if (length(ph_data) > 0) {
        if (isTRUE(verbose)) {
          message("Non-empty response for ID: ", id)
        }

        # Add ID and current timestamp to the data
        ph_data$ID <- id
        ph_data$DateTime <- Sys.time()

        # Append the data to the Physicians data frame
        Physicians <- dplyr::bind_rows(Physicians, ph_data)
        if (isTRUE(verbose)) {
          message("Appended data for ID: ", id)
          message("Total records scraped: ", nrow(Physicians))
        }
      } else {
        if (isTRUE(verbose)) {
          message("Empty response for ID: ", id)
        }
        WrongIDs <- c(WrongIDs, id)
      }
    } else {
      if (isTRUE(verbose)) {
        message("Request failed for ID: ", id)
      }
      WrongIDs <- c(WrongIDs, id)
    }

    # Sleep for a while to prevent server overload
    Sys.sleep(1)
  }

  # Define file names
  physicians_file <- file.path(temp_dir, paste("Physicians_", startID, "-", endID, "_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv", sep = ""))
  wrong_ids_file <- file.path(temp_dir, paste("Wrong_IDs_", startID, "-", tail(WrongIDs, 1), "_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv", sep = ""))

  # Write data frames to CSV files
  write.csv(Physicians, physicians_file, row.names = FALSE)
  write.csv(data.frame(WrongIDs = WrongIDs), wrong_ids_file, row.names = FALSE)

  if (isTRUE(verbose)) {
    message("CSV file for Physicians saved as: ", physicians_file)
    message("CSV file for Wrong IDs saved as: ", wrong_ids_file)
    message("Finished scrape_physicians_data_with_tor.")
  }

  beepr::beep(2)
  return(Physicians)
}

# Example usage with Tor port 9150 (update with your Tor port if different)
# scrape_physicians_data_with_tor(startID = 9045923, endID = 9055923, torPort = 9150)
