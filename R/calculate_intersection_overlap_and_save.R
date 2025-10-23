#' Calculate intersection overlap and save results to shapefiles.
#'
#' This function calculates the intersection between block groups and isochrones for a specific drive time and saves the results to a shapefile.  To
#' ensure accurate area-based calculations, both datasets are temporarily
#' projected to an equal-area CRS before measuring.
#'
#' @param block_groups An sf object representing block groups.
#' @param isochrones_joined An sf object representing isochrones.
#' @param drive_time_minutes The drive time value (in minutes) for which to calculate the
#'   intersection.
#' @param output_dir The directory where the intersection shapefile will be saved.
#' @param verbose Logical; if TRUE, prints status messages while running. Default is FALSE.
#'
#' @return None. The function saves the intersection shapefile and provides logging.
#'
#' @examples
#' calculate_intersection_overlap_and_save(block_groups, isochrones_joined, 30L, "data/shp/")
#'
#' @importFrom sf st_intersection st_write st_area st_transform
#' @importFrom dplyr mutate select left_join coalesce
#' @importFrom stats quantile na.omit
#'
#' @export
#'
calculate_intersection_overlap_and_save <- function(block_groups,
                                                    isochrones_joined,
                                                    drive_time_minutes,
                                                    output_dir,
                                                    verbose = FALSE) {
  # Parameter validation
  if (!inherits(block_groups, "sf")) {
    stop("Error: 'block_groups' must be an sf object.")
  }
  if (!inherits(isochrones_joined, "sf")) {
    stop("Error: 'isochrones_joined' must be an sf object.")
  }
  if (!is.numeric(drive_time_minutes) || length(drive_time_minutes) != 1L || drive_time_minutes < 0) {
    stop("Error: 'drive_time_minutes' must be a single non-negative numeric value.")
  }
  if (!is.character(output_dir)) {
    stop("Error: 'output_dir' must be a character string.")
  }

  # Use an equal-area projection for accurate area calculations
  area_crs <- 5070 # NAD83 / Conus Albers

  # Filter isochrones for the specified drive time
  isochrones_filtered <- isochrones_joined[isochrones_joined$drive_time == drive_time_minutes, , drop = FALSE]

  if (nrow(isochrones_filtered) == 0) {
    stop("Error: no isochrones found for the requested drive time.")
  }

  # Project to an equal-area CRS for area calculations
  block_groups_proj <- sf::st_transform(block_groups, area_crs)
  isochrones_proj <- sf::st_transform(isochrones_filtered, area_crs)

  # Calculate intersection in projected CRS
  intersect <- sf::st_intersection(block_groups_proj, isochrones_proj) %>%
    dplyr::mutate(intersect_area = as.numeric(sf::st_area(.)))

  # Data frame version for joins
  intersect_df <- intersect %>%
    dplyr::select(GEOID, intersect_area) %>%
    sf::st_drop_geometry()

  # Log the progress
  if (isTRUE(verbose)) {
    message(sprintf("Calculating intersection for %s minutes...", drive_time_minutes))
  }

  # Write the intersection shapefile
  output_shapefile <- file.path(output_dir, sprintf("intersect_%s_minutes.shp", drive_time_minutes))
  sf::st_write(sf::st_transform(intersect, 4326), output_shapefile, append = FALSE)
  if (isTRUE(verbose)) {
    message("Intersection calculated and saved successfully.")
  }

  # Merge intersection area by GEOID on projected data
  block_groups_proj <- dplyr::left_join(block_groups_proj, intersect_df, by = "GEOID")

  # Calculate area in all block groups (projected CRS)
  block_groups_proj <- block_groups_proj %>%
    dplyr::mutate(bg_area = as.numeric(sf::st_area(block_groups_proj)))

  # Calculate overlap percent between block groups and isochrones
  block_groups_proj <- block_groups_proj %>%
    dplyr::mutate(
      intersect_area = dplyr::coalesce(intersect_area, 0),
      overlap = intersect_area / bg_area
    )

  # Filter out missing overlap values for quantile calculation
  non_missing_overlap <- stats::na.omit(block_groups_proj$overlap)

  if (!length(non_missing_overlap)) {
    warning("No overlaps were found for the requested drive time.")
    return(invisible(NULL))
  }

  # Summary of the overlap percentiles
  summary_bg <- summary(non_missing_overlap)

  # Print the summary
  if (isTRUE(verbose)) {
    message("Summary of Overlap Percentages for ", drive_time_minutes, " minutes:")
    print(summary_bg)
  }

  # Calculate and print the 50th percentile of overlap percentages
  median <- round(stats::quantile(non_missing_overlap, probs = 0.5), 4) * 100
  if (isTRUE(verbose)) {
    message("50th Percentile of Overlap Percentages: ", median, "%")
  }

  # Calculate and print the 75th percentile of overlap percentages
  p75 <- round(stats::quantile(non_missing_overlap, probs = 0.75), 4) * 100
  if (isTRUE(verbose)) {
    message("75th Percentile of Overlap Percentages: ", p75, "%")
  }

  if (requireNamespace("beepr", quietly = TRUE)) {
    beepr::beep(2)
  }
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
