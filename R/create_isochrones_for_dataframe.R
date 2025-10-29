#' Get isochrones for each point in a dataframe
#'
#' This function retrieves isochrones for each point in a given dataframe by looping
#' over the rows and calling the create_isochrones function for each point.
#'
#' @param input_file A path to the input file containing points for which isochrones are to be retrieved.
#' @param breaks A numeric vector specifying the breaks for categorizing drive times (default is c(1800, 3600, 7200, 10800)).
#' @param output_dir Directory where intermediate `.rds` results are written.
#'   Defaults to a session-specific folder beneath [tempdir()].
#' @param save_interval Number of seconds between automatic checkpoint saves.
#'   Defaults to 240 seconds (~4 minutes).
#' @return A dataframe containing the isochrones data with added 'name' column.
#' @importFrom readr write_rds
#' @importFrom sf st_as_sf st_drop_geometry write_sf
#' @importFrom easyr read.any
#' @importFrom hereR set_key
#' @importFrom janitor clean_names
#' @importFrom progress progress_bar
#' @family mapping
#' @export
#' @examples
#' \dontrun{
#' isochrones_data <- create_isochrones_for_dataframe("points.csv")
#' }
create_isochrones_for_dataframe <- function(
    input_file,
    breaks = c(1800, 3600, 7200, 10800),
    api_key = Sys.getenv("HERE_API_KEY"),
    output_dir = NULL,
    save_interval = 240) {
  #input_file <- "_Recent_Grads_GOBA_NPI_2022a.rds" #for testing;
  #input_file <- "data/test_short_inner_join_postmastr_clinician_data_sf.csv"

  if (api_key == "") stop("HERE API key is required via argument or HERE_API_KEY env var.")

  hereR::set_key(api_key)
  dataframe <- easyr::read.any(input_file)

  # Check if "lat" and "long" columns exist
  if (!all(c("lat", "long") %in% colnames(dataframe))) {
    stop("The dataframe must have 'lat' and 'long' columns.")
  }

  # Validate CRS assumption: lat/long should be in WGS84 bounds (Bug #9 fix)
  invalid_lat <- dataframe$lat < -90 | dataframe$lat > 90
  invalid_lon <- dataframe$long < -180 | dataframe$long > 180

  if (any(invalid_lat, na.rm = TRUE)) {
    n_invalid <- sum(invalid_lat, na.rm = TRUE)
    stop(sprintf(
      "Error: %d latitude values are outside valid WGS84 range [-90, 90]. Check that coordinates are in decimal degrees.",
      n_invalid
    ))
  }

  if (any(invalid_lon, na.rm = TRUE)) {
    n_invalid <- sum(invalid_lon, na.rm = TRUE)
    stop(sprintf(
      "Error: %d longitude values are outside valid WGS84 range [-180, 180]. Check that coordinates are in decimal degrees.",
      n_invalid
    ))
  }

  # Convert dataframe to sf object
  dataframe_sf <- dataframe %>%
    janitor::clean_names() %>%
    sf::st_as_sf(coords = c("long", "lat"), crs = 4326)

  # Ensure it's an sf object
  if (!is(dataframe_sf, "sf")) {
    stop("FYI: The file is not an sf object.")
  }

  dataframe <- dataframe_sf

  processed_isochrones <- list()

  total_rows <- nrow(dataframe)
  if (!total_rows) {
    message("No rows found in input data. Nothing to process.")
  }

  pb <- NULL
  if (total_rows > 0) {
    message(sprintf("Retrieving isochrones for %d point%s...", total_rows, ifelse(total_rows == 1, "", "s")))
    pb <- progress::progress_bar$new(
      format = "  Processing [:bar] :percent | :current/:total | eta: :eta",
      total = total_rows,
      clear = FALSE,
      show_after = 0
    )
  }

  if (!is.numeric(save_interval) || length(save_interval) != 1 || is.na(save_interval) || save_interval <= 0) {
    stop("save_interval must be a positive number of seconds.")
  }

  if (is.null(output_dir)) {
    output_dir <- tyler_tempdir("isochrones", create = TRUE)
  } else if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  run_id <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  rds_path <- file.path(output_dir, paste0("isochrones_progress_", run_id, ".rds"))
  gpkg_path <- file.path(output_dir, paste0("isochrones_progress_", run_id, ".gpkg"))

  last_save <- Sys.time()

  save_snapshot <- function(force = FALSE) {
    if (!length(processed_isochrones)) {
      return(invisible(NULL))
    }

    elapsed <- as.numeric(difftime(Sys.time(), last_save, units = "secs"))
    need_initial <- !file.exists(rds_path) || !file.exists(gpkg_path)
    if (!force && !need_initial && elapsed < save_interval) {
      return(invisible(NULL))
    }

    combined <- do.call(rbind, processed_isochrones)
    readr::write_rds(combined, rds_path)
    sf::write_sf(combined, gpkg_path, delete_dsn = TRUE)

    last_save <<- Sys.time()
    message(sprintf(
      "Auto-saved %d isochrone feature%s to\n  %s\n  %s",
      nrow(combined),
      ifelse(nrow(combined) == 1, "", "s"),
      rds_path,
      gpkg_path
    ))
    invisible(NULL)
  }

  # Loop over the rows in the dataframe
  for (i in seq_len(total_rows)) {

    # Get the point for the current row
    point_temp <- dataframe[i, ]

    # Get isochrones for that point
    Sys.sleep(0.4)
    point_isochrones <- create_isochrones(location = point_temp, range = breaks)
    if (is.list(point_isochrones) && length(point_isochrones) && !is.null(point_isochrones$error)) {
      next
    }

    if (length(point_isochrones)) {
      flattened <- lapply(names(point_isochrones), function(name) {
        iso <- point_isochrones[[name]]
        if (!inherits(iso, "sf")) {
          return(NULL)
        }
        iso$column_label <- name
        iso
      })
      flattened <- Filter(Negate(is.null), flattened)

      if (length(flattened)) {
        point_sf <- do.call(rbind, flattened)
        point_sf$point_index <- i
        point_sf$travel_time_minutes <- point_sf$range / 60
        point_sf$name <- sprintf("%d minutes", as.integer(round(point_sf$travel_time_minutes)))

        attributes_df <- sf::st_drop_geometry(point_temp)
        if (ncol(attributes_df)) {
          repeated_attrs <- attributes_df[rep(1, nrow(point_sf)), , drop = FALSE]
          point_sf <- cbind(point_sf, repeated_attrs)
        }

        processed_isochrones[[length(processed_isochrones) + 1]] <- point_sf
        save_snapshot(force = FALSE)
      }

      # Clean up temporary objects to prevent memory leaks
      rm(point_isochrones, flattened, point_sf, attributes_df, repeated_attrs)
      gc()  # Force garbage collection
    }

    if (!is.null(pb)) {
      pb$tick()
    }
  }

  save_snapshot(force = TRUE)
  beepr::beep(2)

  if (!length(processed_isochrones)) {
    return(data.frame())
  }

  combined_output <- do.call(rbind, processed_isochrones)
  return(combined_output)
}

# Usage example:
#isochrones_data <- create_isochrones_for_dataframe(input_file, breaks = c(1800, 3600, 7200, 10800))
