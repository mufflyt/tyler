#' Create a Base Leaflet Map
#'
#' This function creates a base Leaflet map with a custom title.
#'
#' @param title A character string containing the HTML title for the map.
#'
#' @return A Leaflet map object.
#'
#' @examples
#' \dontrun{
#' # Create a base map with a custom title
#' my_map <- create_base_map("<h1>Custom Map Title</h1>")
#'
#' # Display the map and add circle markers
#' my_map <- my_map %>%
#'   leaflet::addCircleMarkers(lng = ~longitude,
#'                            lat = ~latitude,
#'                            data = data_points,
#'                            popup = ~popup_text,
#'                            radius = ~radius,
#'                            color = ~color,
#'                            fill = TRUE,
#'                            stroke = FALSE,
#'                            fillOpacity = 0.8)
#' }
#'
#' @export
create_base_map <- function(title) {
  # Display setup instructions
  cat("\033[34mInstructions:\033[0m\n")
  cat("\033[34mTo create a base map, use the following code:\033[0m\n")
  cat("\033[34mmy_map <- create_base_map(\"<h1>Custom Map Title</h1>\")\n")
  cat("\033[34mTo display the map and add circle markers, use the following code:\033[0m\n")
  cat("\033[34mmy_map <- my_map %>%\n")
  cat("  leaflet::addCircleMarkers(lng = ~longitude,\n")
  cat("                            lat = ~latitude,\n")
  cat("                            data = data_points,\n")
  cat("                            popup = ~popup_text,\n")
  cat("                            radius = ~radius,\n")
  cat("                            color = ~color,\n")
  cat("                            fill = TRUE,\n")
  cat("                            stroke = FALSE,\n")
  cat("                            fillOpacity = 0.8)\n\n\033[0m")

  # Create the Leaflet map
  lf <- leaflet() %>%
    setView(lat = 39.8282, lng = -98.5795, zoom = 4.5) %>%
    leaflet::clearMarkers() %>%
    leaflet::addControl(title, position = "topleft", className = "map-title") %>%
    leaflet::addScaleBar(position = "bottomleft") %>%
    leaflet::addTiles(options = leaflet::tileOptions(useCache = TRUE, crossOrigin = TRUE)) %>%
    leaflet::addLayersControl(baseGroups = c("Positron", "OpenStreetMap", "Toner Map", "Satellite Map"), options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
    mapOptions(zoomToLimits = "first")

  cat("Setting up the base map...\n")
  cat("Adding map title...\n")
  cat("Adding scale bar...\n")
  cat("Adding base map tiles...\n")
  cat("Adding layer control...\n")

  cat("\nMap setup complete.\n")

  return(lf)
}
