#' Get isochrones for each point in a dataframe
#'
#' This function retrieves isochrones for each point in a given dataframe by looping
#' over the rows and calling the create_isochrones function for each point.
#'
#' @param dataframe A dataframe containing the points for which isochrones are to be retrieved.
#' @param breaks A vector of breaks for labeling the isochrones.
#' @return A dataframe containing the isochrones data with added 'name' column.
#' Create Isochrones for Each Point in a Dataframe
#'
#' This function retrieves isochrones for each point in a given dataframe by looping
#' over the rows and calling the create_isochrones function for each point.
#'
#' @param dataframe A dataframe containing the points for which isochrones are to be retrieved.
#' @param breaks A numeric vector specifying the breaks for categorizing drive times (default is c(1800, 3600, 7200, 10800)).  This allows for 30 minutes, 60 minutes, 120 minutes, and 180 minutes.
#' @return A dataframe containing the isochrones data with added 'name' column.
#' @import dplyr
#' @import readr
#' @import sf
#' @import easyr
#' @export
create_isochrones_for_dataframe <- function(input_file, breaks = c(1800, 3600, 7200, 10800)) {
  # input_file <- "/Users/tylermuffly/Dropbox (Personal)/Tannous/data/references/_Recent_Grads_GOBA_NPI_2022a.rds" #for testing;
  # Sys.setenv(HERE_API_KEY = "VnDX-Rafqchcmb4LUDgEpYlvk8S1-LCYkkrtb1ujOrM")
  # breaks <- c(1800, 3600, 7200, 10800)
  library(tidyverse)
  library(sf)
  library(easyr)

  dataframe <- easyr::read.any(input_file)

  # Check if "lat" and "long" columns exist
  if (!all(c("lat", "long") %in% colnames(dataframe))) {
    stop("The dataframe must have 'lat' and 'long' columns.")
  }

  # Convert dataframe to sf object
  dataframe_sf <- dataframe %>%
    sf::st_as_sf(coords = c("long", "lat"), crs = 4326)

  # Ensure it's an sf object
  if (!is(dataframe_sf, "sf")) {
    stop("FYI: The file is not an sf object.")
  }

  class(dataframe_sf) #for testing
  dataframe <- dataframe_sf %>% head(10)

  # Initialize isochrones as an empty data frame
  isochrones <- data.frame()

  # Loop over the rows in the dataframe
  for (i in 1:nrow(dataframe)) {
    print(i)

    # Get the point for the current row
    point_temp <- dataframe[i, ]

    # Get isochrones for that point
    Sys.sleep(0.4)
    isochrones_temp <- create_isochrones(location = point_temp, range = breaks)

    # If the point errored out, skip it
    if (!is.null(isochrones_temp)) {
      # Flatten the list of isolines
      isochrones_temp <- purrr::flatten_df(isochrones_temp)

      # Create the 'name' column with descriptive labels
      isochrones_temp$name <- cut(
        isochrones_temp$range / 60,
        breaks = breaks,
        labels = paste0(head(breaks, -1), "-", tail(breaks, -1) - 1, " minutes")
      )

      # Combine the list of isolines into the main data frame
      isochrones <- dplyr::bind_rows(isochrones, isochrones_temp)
    }
  }

  # Save the isochrones data to an RDS file
  # readr::write_rds(isochrones, paste("data/isochrones_raw_output_from_here_api_", format(Sys.time(), format = "%Y-%m-%d_%H-%M-%S"), ".rds", sep = ""))

  return(isochrones)
}

# Usage example:
# isochrones_data <- create_isochrones_for_dataframe(gyn_onc, breaks = c(1800, 3600, 7200, 10800))

# gyn_onc <- "/Users/tylermuffly/Dropbox (Personal)/Tannous/data/references/_Recent_Grads_GOBA_NPI_2022a.rds"
# gyn_onc <- readr::read_rds("/Users/tylermuffly/Dropbox (Personal)/Tannous/data/references/_Recent_Grads_GOBA_NPI_2022a.rds") %>%
#   dplyr::filter(sub1=="ONC") %>%
#   dplyr::rename(name = name.x) %>%
#   #sf::st_as_sf(coords = c("long", "lat")) %>%
#   #sf::st_set_crs(4326) %>%
#   dplyr::mutate(unique_id = dplyr::row_number())
#
# gyn_onc_sf <- gyn_onc %>%
#   sf::st_as_sf(coords = c("long", "lat"), crs = 4326)
