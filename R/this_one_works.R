#' Scrape Physicians' Data with Tor
#'
#' This function scrapes data for physicians within a specified ID range, excluding wrong IDs.
#'
#' @param startID The starting ID for scraping.
#' @param endID The ending ID for scraping.
#' @param torPort The port number for Tor SOCKS proxy.
#'
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
scrape_physicians_data_with_tor <- function(startID, endID, torPort) {
  cat("Starting scrape_physicians_data_with_tor...\n")
  cat("Parameters - startID:", startID, "endID:", endID, "torPort:", torPort, "\n")

  # Create a sequence of IDs from startID to endID
  id_list <- seq(startID, endID)
  cat("ID list:", paste(id_list, collapse = ", "), "\n")

  # Load the list of wrong IDs to exclude from scraping
  wrongs1 <- readr::read_csv("~/Desktop/MASTER_all_wrongs.csv")
  wrong_ids <- wrongs1$WrongIDs
  cat("Wrong IDs loaded:", paste(wrong_ids, collapse = ", "), "\n")

  # Initialize data frames to store physicians' data and wrong IDs
  Physicians <- data.frame()
  WrongIDs <- c()
  cat("Initialized empty data frames for Physicians and WrongIDs.\n")

  # API Inputs
  base <- "https://api.abog.org/"
  endpoint <- "diplomate/"
  action <- "/verify"

  for (id in id_list) {
    cat("Processing ID:", id, "\n")

    # Check if the ID is in the list of wrong IDs
    if (id %in% wrong_ids) {
      cat("Skipping wrong ID:", id, "\n")
      next
    }

    # Create the API URL for the current ID
    url <- paste(base, endpoint, id, action, sep = "")
    cat("API URL:", url, "\n")

    # Send a GET request through Tor
    ph_r <- httr::GET(url, use_proxy(paste0("socks5://localhost:", torPort)))

    # Check if the request was successful
    if (ph_r$status_code == 200) {
      cat("Request successful for ID:", id, "\n")

      # Parse JSON response
      ph_data <- jsonlite::fromJSON(httr::content(ph_r, as = "text"))

      # Check if the response is not empty
      if (length(ph_data) > 0) {
        cat("Non-empty response for ID:", id, "\n")

        # Add ID and current timestamp to the data
        ph_data$ID <- id
        ph_data$DateTime <- Sys.time()

        # Append the data to the Physicians data frame
        Physicians <- dplyr::bind_rows(Physicians, ph_data)
        cat("Appended data for ID:", id, "\n")
        cat("Total records scraped:", nrow(Physicians), "\n")
      } else {
        cat("Empty response for ID:", id, "\n")
        WrongIDs <- c(WrongIDs, id)
      }
    } else {
      cat("Request failed for ID:", id, "\n")
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

  cat("CSV file for Physicians saved as:", physicians_file, "\n")
  cat("CSV file for Wrong IDs saved as:", wrong_ids_file, "\n")

  cat("Finished scrape_physicians_data_with_tor.\n")

  return(Physicians)
}

# Example usage with Tor port 9150 (update with your Tor port if different)
# scrape_physicians_data_with_tor(startID = 9045923, endID = 9055923, torPort = 9150)
