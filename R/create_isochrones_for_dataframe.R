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

  Sys.setenv(HERE_API_KEY = "VnDX-Rafqchcmb4LUDgEpYlvk8S1-LCYkkrtb1ujOrM")
  readRenviron("~/.Renviron")
  hereR::set_key("VnDX-Rafqchcmb4LUDgEpYlvk8S1-LCYkkrtb1ujOrM")

  library(tidyverse)
  library(sf)
  library(easyr)

  dataframe <- easyr::read.any(input_file) %>%
    filter(!is.na(lat) | !is.na(long))


  # Check if "lat" and "long" columns exist
  if (!all(c("lat", "long") %in% colnames(dataframe))) {
    stop("The dataframe must have 'lat' and 'long' columns.")
  }

  # Convert dataframe to sf object
  dataframe_sf <- dataframe %>%
    janitor::clean_names()%>%
    sf::st_as_sf(coords = c("long", "lat"), crs = 4326)

  # Ensure it's an sf object
  if (!is(dataframe_sf, "sf")) {
    stop("FYI: The file is not an sf object.")
  }

  class(dataframe_sf) #for testing
  dataframe <- dataframe_sf

  # Initialize isochrones as an empty data frame
  isochrones <- list()
  isochrones_temp <- list()

  # Loop over the rows in the dataframe
  for (i in 1:nrow(dataframe)) {
    print(i)

    # Get the point for the current row
    point_temp <- dataframe[i, ]

    # Get isochrones for that point
    Sys.sleep(0.4)
    isochrones_temp[[i]] <- create_isochrones(location = point_temp, range = breaks)
    if (!is.null(isochrones_temp[[i]])) {
      # Flatten the list of isolines
      isochrones_temp[[i]] <- dplyr::bind_rows(isochrones_temp[[i]], .id = "column_label")

      # Create the 'name' column with descriptive labels
      isochrones_temp[[i]]$name <- cut(
        isochrones_temp[[i]]$range / 60,
        breaks = breaks,
        labels = paste0(head(breaks, -1), "-", tail(breaks, -1) - 1, " minutes")
      )


    }

  }
  isochrones <- data.table::rbindlist(isochrones_temp)
  return(isochrones)
}
# Usage example:
#isochrones_data <- create_isochrones_for_dataframe(input_file, breaks = c(1800, 3600, 7200, 10800))
