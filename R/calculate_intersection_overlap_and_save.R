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
#' @param crosswalk Optional function used to translate ACS geographies between
#'   2010 and 2020 definitions. When supplied, it should accept two arguments:
#'   an sf object and a named list with `from` and `to` years, returning an sf
#'   object whose `vintage` column matches the provider `data_year`.
#' @param notify Logical. If `TRUE`, play a notification sound on completion when
#'   the optional `beepr` package is available. Defaults to `TRUE`.
#'
#' @return None. The function saves the intersection shapefile and provides logging.
#'
#' @examples
#' calculate_intersection_overlap_and_save(block_groups, isochrones_joined, 30L, "data/shp/")
#'
#' @importFrom sf st_intersection st_write st_area st_transform st_make_valid st_is_valid st_union st_sf
#' @importFrom lwgeom st_orient
#' @importFrom dplyr mutate select left_join coalesce
#' @importFrom rlang .data
#' @importFrom stats quantile na.omit
#'
#' @export
#'
calculate_intersection_overlap_and_save <- function(block_groups,
                                                    isochrones_joined,
                                                    drive_time_minutes,
                                                    output_dir,
                                                    crosswalk = NULL,
                                                    notify = TRUE) {
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

  block_groups <- sf::st_make_valid(block_groups)
  isochrones_joined <- sf::st_make_valid(isochrones_joined)

  if (!all(sf::st_is_valid(block_groups))) {
    stop("Block group geometries remain invalid after attempted repair.")
  }
  if (!all(sf::st_is_valid(isochrones_joined))) {
    stop("Isochrone geometries remain invalid after attempted repair.")
  }

  block_groups <- lwgeom::st_orient(block_groups)
  isochrones_joined <- lwgeom::st_orient(isochrones_joined)

  # Year alignment enforcement
  if (!"data_year" %in% names(isochrones_joined)) {
    stop("`isochrones_joined` must include a `data_year` column for provider vintage alignment.")
  }
  if (!"vintage" %in% names(block_groups)) {
    stop("`block_groups` must include a `vintage` column indicating ACS vintage.")
  }

  provider_years <- stats::na.omit(unique(isochrones_joined$data_year))
  acs_years <- stats::na.omit(unique(block_groups$vintage))

  if (length(provider_years) != 1L) {
    stop("`isochrones_joined$data_year` must contain a single, non-missing year value.")
  }
  if (length(acs_years) != 1L) {
    stop("`block_groups$vintage` must contain a single, non-missing year value.")
  }

  provider_year <- provider_years[[1]]
  acs_year <- acs_years[[1]]

  if (!identical(provider_year, acs_year)) {
    if (!identical(sort(c(provider_year, acs_year)), c(2010, 2020))) {
      stop(
        sprintf(
          "Provider data_year (%s) and ACS vintage (%s) differ. Provide matching vintages.",
          provider_year,
          acs_year
        )
      )
    }

    if (is.null(crosswalk)) {
      stop(
        sprintf(
          paste(
            "Provider data_year (%s) and ACS vintage (%s) differ. ",
            "Supply a `crosswalk` function to translate between 2010 and 2020 geographies."
          ),
          provider_year,
          acs_year
        )
      )
    }

    block_groups <- crosswalk(block_groups, list(from = acs_year, to = provider_year))

    if (!inherits(block_groups, "sf")) {
      stop("`crosswalk` must return an sf object.")
    }
    if (!"vintage" %in% names(block_groups)) {
      stop("The object returned by `crosswalk` must include a `vintage` column.")
    }

    block_groups <- sf::st_make_valid(block_groups)
    if (!all(sf::st_is_valid(block_groups))) {
      stop("Crosswalk output contains invalid geometries that could not be repaired.")
    }

    block_groups <- lwgeom::st_orient(block_groups)

    acs_years <- stats::na.omit(unique(block_groups$vintage))
    if (length(acs_years) != 1L || !identical(acs_years[[1]], provider_year)) {
      stop("`crosswalk` did not return ACS data aligned to the provider year.")
    }
  }

  # Use an equal-area projection for accurate area calculations
  area_crs <- 5070 # NAD83 / Conus Albers

  # Filter isochrones for the specified drive time
  isochrones_filtered <- isochrones_joined[isochrones_joined$drive_time == drive_time_minutes, , drop = FALSE]

  if (nrow(isochrones_filtered) == 0) {
    stop("Error: no isochrones found for the requested drive time.")
  }

  isochrones_filtered <- sf::st_union(isochrones_filtered)
  isochrones_filtered <- sf::st_sf(
    drive_time = drive_time_minutes,
    data_year = provider_year,
    geometry = isochrones_filtered,
    crs = sf::st_crs(isochrones_joined)
  )
  isochrones_filtered <- lwgeom::st_orient(isochrones_filtered)

  # Project to an equal-area CRS for area calculations
  block_groups_proj <- sf::st_transform(block_groups, area_crs)
  isochrones_proj <- sf::st_transform(isochrones_filtered, area_crs)

  # Calculate intersection in projected CRS
  intersect <- sf::st_intersection(block_groups_proj, isochrones_proj) %>%
    dplyr::mutate(
      intersect_area = as.numeric(sf::st_area(.)),
      area_method = "projected:EPSG:5070"
    )

  # Data frame version for joins
  intersect_df <- intersect %>%
    dplyr::select(GEOID, intersect_area, area_method) %>%
    sf::st_drop_geometry()

  # Log the progress
  message(sprintf("Calculating intersection for %s minutes...", drive_time_minutes))

  # Write the intersection shapefile
  output_shapefile <- file.path(output_dir, sprintf("intersect_%s_minutes.shp", drive_time_minutes))
  sf::st_write(sf::st_transform(intersect, 4326), output_shapefile, append = FALSE)
  message("Intersection calculated and saved successfully.")

  # Merge intersection area by GEOID on projected data
  block_groups_proj <- dplyr::left_join(block_groups_proj, intersect_df, by = "GEOID")

  # Calculate area in all block groups (projected CRS)
  block_groups_proj <- block_groups_proj %>%
    dplyr::mutate(
      bg_area = as.numeric(sf::st_area(block_groups_proj)),
      area_method = dplyr::coalesce(rlang::.data$area_method, "projected:EPSG:5070")
    )

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
  message("Summary of Overlap Percentages for ", drive_time_minutes, " minutes:")
  print(summary_bg)

  # Calculate and print the 50th percentile of overlap percentages
  median <- round(stats::quantile(non_missing_overlap, probs = 0.5), 4) * 100
  message("50th Percentile of Overlap Percentages: ", median, "%")

  # Calculate and print the 75th percentile of overlap percentages
  p75 <- round(stats::quantile(non_missing_overlap, probs = 0.75), 4) * 100
  message("75th Percentile of Overlap Percentages: ", p75, "%")

  if (isTRUE(notify) && requireNamespace("beepr", quietly = TRUE)) {
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
