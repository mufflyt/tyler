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
#' @param breaks A numeric vector specifying the breaks for categorizing drive times (default is c(0, 30, 60, 120, 180)).
#' @return A dataframe containing the isochrones data with added 'name' column.
#' @import dplyr
#' @import readr
#' @import sf
#' @export
create_isochrones_for_dataframe <- function(dataframe, breaks = c(0, 30, 60, 120, 180)) {
  # Initialize isochrones as an empty NULL
  isochrones <- NULL

  # Loop over the rows in the dataframe
  for (i in 1:nrow(dataframe)) {
    print(i)

    # Get the point for the current row
    point_temp <- dataframe[i, ]

    # Get isochrones for that point
    Sys.sleep(0.4)
    isochrones_temp <- create_isochrones(location = point_temp, range = c(1800, 3600, 7200, 10800))

    # If the point errored out, skip it
    if (!is.null(isochrones_temp)) {
      # Create the 'name' column with descriptive labels
      isochrones_temp$name <- cut(
        isochrones_temp$range / 60,
        breaks = breaks,
        labels = paste0(head(breaks, -1), "-", tail(breaks, -1) - 1, " minutes")
      )

      # Combine the list of isochrones into a single data frame
      isochrones <- dplyr::bind_rows(isochrones, isochrones_temp)
    }
  }

  # Save the isochrones data to an RDS file
  readr::write_rds(isochrones, paste("data/isochrones_raw_output_from_here_api_", format(Sys.time(), format = "%Y-%m-%d_%H-%M-%S"), ".rds", sep = ""))

  return(isochrones)
}

# Usage example:
# isochrones_data <- create_isochrones_for_dataframe(gyn_onc, breaks = c(0, 30, 60, 120, 180))
