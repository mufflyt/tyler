#' Create and Save a Leaflet Dot Map of Physicians
#'
#' This function creates a Leaflet dot map of physicians using their longitude and latitude
#' coordinates. It also adds ACOG district boundaries to the map and saves it as an HTML file
#' with an accompanying PNG screenshot.
#'
#' @param physician_data An sf object containing physician data with "long" and "lat" columns.
#' @param jitter_range The range for adding jitter to latitude and longitude coordinates.
#' @param color_palette The color palette for ACOG district colors.
#' @param popup_var The variable to use for popup text.
#' @return A Leaflet map object.
#'
#' @import viridis
#' @import leaflet
#' @import webshot
#' @import htmlwidgets
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' # Load required libraries
#' library(viridis)
#' library(leaflet)
#'
#' # Generate physician data (replace with your own data)
#' physician_data <- data.frame(
#'   long = c(-95.363271, -97.743061, -98.493628, -96.900115, -95.369803),
#'   lat = c(29.763283, 30.267153, 29.424349, 32.779167, 29.751808),
#'   name = c("Physician 1", "Physician 2", "Physician 3", "Physician 4", "Physician 5"),
#'   ACOG_District = c("District I", "District II", "District III", "District IV", "District V")
#' )
#'
#' # Create and save the dot map
#' create_and_save_physician_dot_map(physician_data)
#' }
#'
#' @export

# Here's a summary of what the function does:
# Sets up the base map and generates ACOG district boundaries.
# Creates a Leaflet map with physician markers that have a black outline.
# Adds a timestamp to the file names for HTML and PNG files.
# Saves the Leaflet map as an HTML file with a timestamp in the file name.
# Captures and saves a screenshot as a PNG file with a timestamp in the file name.
# Returns the Leaflet map.

create_and_save_physician_dot_map <- function(physician_data, jitter_range = 0.05, color_palette = "magma", popup_var = "name") {
  # Add jitter to latitude and longitude coordinates
  jittered_physician_data <- physician_data %>%
    dplyr::mutate(
      lat = lat + runif(n()) * jitter_range,
      long = long + runif(n()) * jitter_range
    )

  # Create a base map using tyler::create_base_map()
  cat("Setting up the base map...\n")
  base_map <- tyler::create_base_map("Physician Dot Map")
  cat("Map setup complete.\n")

  # Generate ACOG districts using tyler::generate_acog_districts_sf()
  cat("Generating the ACOG district boundaries from tyler::generate_acog_districts_sf...\n")
  acog_districts <- tyler::generate_acog_districts_sf()

  # Define the number of ACOG districts
  num_acog_districts <- 11

  # Create a custom color palette using viridis
  district_colors <- viridis::viridis(num_acog_districts, option = color_palette)

  # Reorder factor levels
  jittered_physician_data$ACOG_District <- factor(
    jittered_physician_data$ACOG_District,
    levels = c("District I", "District II", "District III", "District IV", "District V",
               "District VI", "District VII", "District VIII", "District IX",
               "District XI", "District XII"))

  # Create a Leaflet map
  dot_map <- base_map %>%
    # Add physician markers
    leaflet::addCircleMarkers(
      data = jittered_physician_data,
      lng = ~long,
      lat = ~lat,
      radius = 3,         # Adjust the radius as needed
      stroke = TRUE,      # Add a stroke (outline)
      weight = 1,         # Adjust the outline weight as needed
      color = district_colors[as.numeric(physician_data$ACOG_District)],   # Set the outline color to black
      fillOpacity = 0.8,  # Fill opacity
      popup = as.formula(paste0("~", popup_var))  # Popup text based on popup_var argument
    ) %>%
    # Add ACOG district boundaries
    leaflet::addPolygons(
      data = acog_districts,
      color = "red",      # Boundary color
      weight = 2,         # Boundary weight
      fill = FALSE,       # No fill
      opacity = 0.8,      # Boundary opacity
      popup = ~ACOG_District   # Popup text
    ) %>%
    # Add a legend
    leaflet::addLegend(
      position = "bottomright",   # Position of the legend on the map
      colors = district_colors,   # Colors for the legend
      labels = levels(physician_data$ACOG_District),   # Labels for legend items
      title = "ACOG Districts"   # Title for the legend
    )

  # Generate a timestamp
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

  # Define file names with timestamps
  html_file <- paste0("figures/dot_map_", timestamp, ".html")
  png_file <- paste0("figures/dot_map_", timestamp, ".png")

  # Save the Leaflet map as an HTML file
  htmlwidgets::saveWidget(widget = dot_map, file = html_file, selfcontained = TRUE)
  cat("Leaflet map saved as HTML:", html_file, "\n")

  # Capture and save a screenshot as PNG
  webshot::webshot(html_file, file = png_file)
  cat("Screenshot saved as PNG:", png_file, "\n")

  # Return the Leaflet map
  return(dot_map)
}
