#' Calculate intersection overlap and save results to shapefiles.
#'
#' This function calculates the intersection between block groups and isochrones for a specific drive time and saves the results to a shapefile.
#'
#' @param block_groups An sf object representing block groups.
#' @param isochrones_joined An sf object representing isochrones.
#' @param drive_time The drive time value for which to calculate the intersection.
#' @param output_dir The directory where the intersection shapefile will be saved.
#'
#' @return None. The function saves the intersection shapefile and provides logging.
#'
#' @examples
#' calculate_intersection_overlap_and_save(block_groups, isochrones_joined, 30L, "data/shp/")
#'
#' @importFrom sf st_intersection st_write st_area
#' @importFrom dplyr mutate select left_join
#'
#' @export
#'
calculate_intersection_overlap_and_save <- function(block_groups, isochrones_joined, drive_time, output_dir) {
  # Parameter validation
  if (!inherits(block_groups, "sf")) {
    stop("Error: 'block_groups' must be an sf object.")
  }
  if (!inherits(isochrones_joined, "sf")) {
    stop("Error: 'isochrones_joined' must be an sf object.")
  }
  if (!is.numeric(drive_time) || drive_time < 0) {
    stop("Error: 'drive_time' must be a non-negative numeric value.")
  }
  if (!is.character(output_dir)) {
    stop("Error: 'output_dir' must be a character string.")
  }

  # Load necessary packages and functions
  library(sf)
  library(dplyr)

  # Filter isochrones for the specified drive time
  isochrones_filtered <- sf::filter(isochrones_joined, drive_time == drive_time)

  # Calculate intersection
  intersect <- sf::st_intersection(block_groups, isochrones_filtered) %>%
    dplyr::mutate(intersect_area = sf::st_area(.)) %>%
    dplyr::select(GEOID, intersect_area) %>%
    sf::st_drop_geometry()

  # Log the progress
  message(paste("Calculating intersection for", drive_time, "minutes..."))

  tryCatch(
    {
      # Write the intersection shapefile
      output_shapefile <- file.path(output_dir, paste0("intersect_", drive_time, "_minutes.shp"))
      sf::st_write(intersect, output_shapefile, append = FALSE)
      message("Intersection calculated and saved successfully.")

      # Merge intersection area by GEOID
      block_groups <- dplyr::left_join(block_groups, intersect, by = "GEOID")

      # Calculate area in all block groups
      block_groups <- block_groups %>%
        dplyr::mutate(bg_area = sf::st_area(block_groups))

      # Calculate overlap percent between block groups and isochrones
      block_groups <- block_groups %>%
        dplyr::mutate(
          intersect_area = ifelse(is.na(intersect_area), 0, intersect_area),
          overlap = as.numeric(intersect_area / bg_area)
        )

      # Filter out missing overlap values for quantile calculation
      non_missing_overlap <- block_groups$overlap

      # Summary of the overlap percentiles
      summary_bg <- summary(non_missing_overlap)

      # Print the summary
      message("Summary of Overlap Percentages for", drive_time, "minutes:")
      cat(summary_bg)

      # Calculate and print the 50th percentile of overlap percentages
      median <- round(quantile(non_missing_overlap, probs = 0.5), 4) * 100
      message("50th Percentile of Overlap Percentages:", median, "%")

      # Calculate and print the 75th percentile of overlap percentages
      mean <- round(mean(non_missing_overlap), 4) * 100
      message("75th Percentile of Overlap Percentages:", mean, "%")

    },
    error = function(e) {
      message("Error: ", e)
    }
  )
}


# # Use Case
# # List of unique drive times for which you want to calculate intersection
# unique_drive_times <- unique(isochrones_filtered$drive_time)
#
# # Specify the output directory
# output_dir <- "data/shp/"
#
# # Loop through unique drive times and calculate intersection for each
# for (drive_time in unique_drive_times) {
#   calculate_intersection_overlap_and_save(block_groups, isochrones_joined, drive_time, output_dir)
# }
