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
#' @import leaflet
#' @export
create_base_map <- function(title) {
  # Create the Leaflet map
  lf <- leaflet::leaflet() %>%
    addProviderTiles("CartoDB.PositronNoLabels") %>%
    leaflet::setView(lat = 39.8282, lng = -98.5795, zoom = 3) %>%
    leaflet::clearMarkers() %>%
    leaflet::addControl(title, position = "topleft", className = "map-title") %>%
    addScaleBar(position = "bottomleft", options = scaleBarOptions(metric = FALSE, maxWidth = 200)) %>%
    leaflet::addTiles(options = leaflet::tileOptions(useCache = TRUE, crossOrigin = TRUE))

  cat("Setting up the base map...\n")
  cat("Adding map title...\n")
  cat("Adding scale bar...\n")
  cat("Adding base map tiles...\n")

  cat("\nMap setup complete.\n")

  return(lf)
}
