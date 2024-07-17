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
#' @importFrom viridis viridis
#' @importFrom leaflet addCircleMarkers addPolygons addLegend
#' @importFrom webshot webshot
#' @importFrom htmlwidgets saveWidget
#' @importFrom dplyr mutate
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
create_and_save_physician_dot_map <- function(physician_data, jitter_range = 0.05, color_palette = "magma", popup_var = "name") {
  # Add jitter to latitude and longitude coordinates
  jittered_physician_data <- dplyr::mutate(physician_data,
                                           lat = lat + runif(n()) * jitter_range,
                                           long = long + runif(n()) * jitter_range)

  # Create a base map using tyler::create_base_map()
  cat("Setting up the base map...\n")
  base_map <- tyler::create_base_map("Physician Dot Map")
  cat("Map setup complete.\n")

  # Generate ACOG districts using tyler::generate_acog_districts_sf()
  cat("Generating the ACOG district boundaries...\n")
  acog_districts <- tyler::generate_acog_districts_sf()
  cat("ACOG district boundaries generated.\n")

  # Define the number of ACOG districts
  num_acog_districts <- 11

  # Create a custom color palette using viridis
  district_colors <- viridis::viridis(num_acog_districts, option = color_palette)

  # Reorder factor levels
  jittered_physician_data <- dplyr::mutate(jittered_physician_data,
                                           ACOG_District = factor(ACOG_District,
                                                                  levels = c("District I", "District II", "District III", "District IV", "District V",
                                                                             "District VI", "District VII", "District VIII", "District IX",
                                                                             "District XI", "District XII")))

  # Create a Leaflet map
  dot_map <- leaflet::addCircleMarkers(base_map,
                                       data = jittered_physician_data,
                                       lng = ~long,
                                       lat = ~lat,
                                       radius = 3,
                                       stroke = TRUE,
                                       weight = 1,
                                       color = district_colors[as.numeric(physician_data$ACOG_District)],
                                       fillOpacity = 0.8,
                                       popup = as.formula(paste0("~", popup_var))) %>%
    leaflet::addPolygons(data = acog_districts,
                         color = "red",
                         weight = 2,
                         fill = FALSE,
                         opacity = 0.8,
                         popup = ~ACOG_District) %>%
    leaflet::addLegend(position = "bottomright",
                       colors = district_colors,
                       labels = levels(physician_data$ACOG_District),
                       title = "ACOG Districts")

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
