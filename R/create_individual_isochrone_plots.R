#' Create Individual Isochrone Maps and Shapefiles
#'
#' This function creates individual Leaflet maps and shapefiles for specified drive times
#' based on isochrone data.
#'
#' @param isochrones An sf object containing isochrone data.
#' @param drive_times A vector of unique drive times (in minutes) for which maps and shapefiles will be created.
#' @param html_dir Directory where HTML maps will be saved. Defaults to
#'   "figures/isochrone_maps" in the current working directory.
#' @param shapefile_dir Directory where shapefiles will be saved. Defaults to
#'   "data/shp/isochrone_files" in the current working directory.
#' @return None. The function creates and saves individual maps and shapefiles.
#'
#' @importFrom sf st_union st_sf st_transform st_write
#' @importFrom leaflet addProviderTiles addPolygons
#' @importFrom dplyr filter tibble
#' @importFrom grDevices rainbow
#' @importFrom htmlwidgets saveWidget
#'
#' @examples
#' \dontrun{
#' # Load required libraries
#' library(sf)
#' library(leaflet)
#' library(tyler)
#'
#' # Load isochrone data
#' isochrones <- readRDS("path_to_isochrones.rds")
#'
#' # List of unique drive times for which you want to create plots and shapefiles
#' drive_times <- unique(isochrones$drive_time)
#'
#' # Create individual isochrone maps and shapefiles
#' create_individual_isochrone_plots(isochrones, drive_times)
#' }
#'
#' @family mapping
#' @export
create_individual_isochrone_plots <- function(isochrones, drive_times,
                                              html_dir = file.path(getwd(), "figures/isochrone_maps"),
                                              shapefile_dir = file.path(getwd(), "data/shp/isochrone_files")) {
  # Display setup instructions
  cat("\033[34mInstructions:\033[0m\n")
  cat("\033[34mTo use this function, follow the example code below:\033[0m\n")
  cat("\n")
  cat("\033[34m# Load isochrone data:\033[0m\n")
  cat("\033[34misochrones <- readRDS(\"path_to_isochrones.rds\")\n")
  cat("\n")
  cat("\033[34m# List of unique drive times for which you want to create plots and shapefiles:\033[0m\n")
  cat("\033[34mdrive_times <- unique(isochrones$drive_time)\n")
  cat("\n")
  cat("\033[34m# Create individual isochrone maps and shapefiles:\033[0m\n")
  cat("\033[34mcreate_individual_isochrone_plots(isochrones, drive_times)\n")

  message("Creating individual isochrone plots and shapefiles...")

  for (time in drive_times) {
    message(paste("Processing isochrones for", time, "minutes..."))

    # Filter isochrones for the specified drive time
    isochrones_filtered <- dplyr::filter(isochrones, drive_time == time)

    # Combine isochrones using st_union
    isochrones_combined <- sf::st_union(isochrones_filtered)

    # Create an sf object with the combined isochrones
    isochrones_sf <- dplyr::tibble(iso_id = 1, geometry = isochrones_combined) %>%
      sf::st_sf()

    # Transform the sf object to an appropriate coordinate system (e.g., EPSG:4326)
    isochrones_sf <- sf::st_transform(isochrones_sf, crs = 4326)

    # Define unique colors for each drive time
    colors <- grDevices::rainbow(length(drive_times))

    # Get the index of the current drive time
    index <- which(drive_times == time)

    # Create a base map
    my_map <- tyler::create_base_map("")

    message(paste("Creating a Leaflet map of isochrones for", time, "minutes..."))

    # Create the Leaflet plot
    isochrone_map <- my_map %>%
      leaflet::addProviderTiles("CartoDB.Voyager") %>%
      leaflet::addPolygons(
        data = isochrones_sf,
        fillColor = colors[index],
        fillOpacity = 1,
        weight = 0.5,
        smoothFactor = 0.2,
        stroke = TRUE,
        color = "black"
      )

    if (!dir.exists(html_dir)) {
      dir.create(html_dir, recursive = TRUE)
    }
    output_file <- file.path(html_dir, paste0("isochrone_map_", time, "_minutes.html"))
    htmlwidgets::saveWidget(isochrone_map, file = output_file)

    message(paste("Saved isochrone map for", time, "minutes as:", output_file))

    # Write the shapefile for the current drive time
    if (!dir.exists(shapefile_dir)) {
      dir.create(shapefile_dir, recursive = TRUE)
    }
    output_shapefile <- file.path(shapefile_dir, paste0("isochrones_", time, "_minutes.shp"))
    sf::st_write(isochrones_sf, output_shapefile, append = FALSE)

    message(paste("Saved shapefile for", time, "minutes as:", output_shapefile))

    message(paste("Processed isochrones for", time, "minutes."))
  }

  message("Individual isochrone plots and shapefiles creation completed.")
}
