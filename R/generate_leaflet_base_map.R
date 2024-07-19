#' Generate Leaflet Base Map
#'
#' This function creates a Leaflet BASE map with specific configurations, including the base tile layer, scale bar,
#' default view settings, and layers control.
#'
#' @return A Leaflet map object.
#' @importFrom leaflet leaflet addProviderTiles clearBounds clearMarkers addScaleBar setView addLayersControl addTiles
#' @export
#' @examples
#' map <- generate_leaflet_base_map()
generate_leaflet_base_map <- function() {
  # Create a new Leaflet map object
  map <- leaflet::leaflet() %>%
    # Add CartoDB Voyager tiles as the base tile layer
    leaflet::addProviderTiles("CartoDB.Voyager", group = "CartoDB Voyager") %>%
    # Clear any previously set bounds
    leaflet::clearBounds() %>%
    # Clear any previously set markers
    leaflet::clearMarkers() %>%
    # Add a scale bar to the bottom-left corner of the map
    leaflet::addScaleBar(position = "bottomleft") %>%
    # Set the initial view of the map to specific latitude, longitude, and zoom level
    leaflet::setView(lat = 39.8282, lng = -98.5795, zoom = 3) %>%
    # Add a layers control for selecting different base layers
    leaflet::addLayersControl(
      baseGroups = c("CartoDB Voyager", "Toner by Stamen"),  # Base layer options
      options = leaflet::layersControlOptions(
        collapsed = FALSE,  # Keep the layers control expanded
        zoomToLimits = TRUE  # Zoom to the extent of the selected layer
      ),
      overlayGroups = NULL  # No overlay groups for this map
    ) %>%
    # Add default map tiles with caching and cross-origin support
    leaflet::addTiles(options = leaflet::tileOptions(useCache = TRUE, crossOrigin = TRUE))

  return(map)
}
