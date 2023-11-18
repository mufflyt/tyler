#' Scrape Physicians' Data
#'
#' This function scrapes data for physicians within a specified ID range, excluding wrong IDs.
#'
#' @param startID The starting ID for scraping.
#' @param endID The ending ID for scraping.
#'
#' @return A dataframe containing scraped physicians' data.
#'
#'
#' @examples
#' # Call the function
#' scrape_result <- scrape_physicians_data(startID = 9045999, endID = 9046000)
#'
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import memoise
scrape_physicians_data <- function(startID, endID) {
  require(httr)
  require(jsonlite)
  require(dplyr)
  require(memoise)

  # Load the data for wrong IDs
  wrongs1 <- readr::read_csv("/Users/tylermuffly/Dropbox (Personal)/workforce/MASTER_all_wrongs.csv") %>%
    select(WrongIDs) %>% arrange(WrongIDs) %>% filter(WrongIDs < 9044000)

  # Create a list of wrong IDs to exclude from scraping
  excluded_ids <- as.integer(wrongs1$WrongIDs)

  # Generate the list of IDs to scrape, excluding wrong IDs
  id_list <- seq(startID, endID)

  # Filter out excluded IDs using base::subset
  id_list <- subset(id_list, !id_list %in% excluded_ids)

  fc <- cache_filesystem(file.path(".cache"))
  GET_m <- memoise::memoise(httr::GET, cache = fc)

  # Initialize the list to store physicians' data
  Physicians <- list()  # Change this to a list

  # Initialize the vector to store wrong IDs
  WrongIDs <- NULL

  for (id in id_list) {
    r <- paste("https://api.abog.org/diplomate/", id, "/verify", sep = "")
    ph_r <- GET_m(r)
    ph_json <- httr::content(x = ph_r, as = "text", encoding = "UTF-8")
    ph_data <- jsonlite::fromJSON(ph_json, flatten = TRUE)

    if (length(ph_data) != 0) {
      # Check if 'userid' column is present in the data
      if (!"userid" %in% colnames(ph_data)) {
        # If 'userid' is missing, create a placeholder
        ph_data$userid <- NA
      }

      ph_data <- ph_data %>%
        dplyr::mutate(ID = id) %>%
        dplyr::mutate(DateTime = Sys.time())

      Physicians <- list(Physicians, ph_data)  # Append the data to the list

      # Debugging: Print ph_data
      print(head(ph_data))
    } else {
      WrongIDs <- c(WrongIDs, id)
    }

    Sys.sleep(0.5)
  }

  # Bind the list elements together into a single dataframe
  Physicians <- dplyr::bind_rows(Physicians)

  # Define the file name for Physicians
  file_name_physicians <- paste("Physicians (", startID, "-", endID, ") (", Sys.time(), ").csv", sep = "")

  # Define the full file path for Physicians
  file_path_physicians <- "data/"  # Replace with the actual directory path where you want to save the Physicians file
  full_file_path_physicians <- file.path(file_path_physicians, file_name_physicians)

  # Save the CSV file for Physicians
  write.csv(Physicians, full_file_path_physicians, row.names = FALSE)

  # Define the file name for WrongIDs
  file_name_wrongids <- paste("Wrong IDs (", startID, "-", tail(WrongIDs, 1), ") (", Sys.time(), ").csv", sep = "")

  # Define the full file path for WrongIDs
  file_path_wrongids <- "data/"  # Replace with the actual directory path where you want to save the WrongIDs file
  full_file_path_wrongids <- file.path(file_path_wrongids, file_name_wrongids)

  # Save the CSV file for WrongIDs
  write.csv(data.frame(WrongIDs = WrongIDs), full_file_path_wrongids, row.names = FALSE)

  # Display a message with the name and path of the saved files
  cat("CSV file for Physicians saved as:", full_file_path_physicians, "\n")
  cat("CSV file for Wrong IDs saved as:", full_file_path_wrongids, "\n")

  # Return the Physicians data as a dataframe
  return(Physicians)
}
