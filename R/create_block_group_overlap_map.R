#' Function to create and export a map showing block group overlap with isochrones
#'
#' This function creates a map that displays block groups and their overlap with isochrones.
# The map is exported as an HTML file and a PNG image.
#
#' @param bg_data A SpatialPolygonsDataFrame representing block group data.
#' @param isochrones_data A SpatialPolygonsDataFrame representing isochrone data.
#' @param output_html File path for exporting the map as an HTML file.
#' @param output_png File path for exporting the map as a PNG image.
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' # Define output file paths
#' output_html <- "figures/overlap_bg_map.html"
#' output_png <- "figures/overlap_bg_map.png"
#'
#' # Create and export the map
#' create_block_group_overlap_map(block_groups, isochrones_joined_map, output_html, output_png)
#'
#' # Call the create_block_group_overlap_map function with your data
#' create_block_group_overlap_map(
#'   gyn_onc_physicians_df = centers_ms,
#'   isochrones_sf = isochrones_combined,
#'   file_name = html_file_name)
#' }
#'
#' @importFrom leaflet leaflet addProviderTiles setView addPolygons addLegend
#' @importFrom leafletExtras colorNumeric
#' @import webshot
#' @importFrom htmlwidgets saveWidget
#'
#' @export
create_block_group_overlap_map <- function(bg_data, isochrones_data, output_html, output_png) {
  bg_data <- bg_data %>% st_transform(4326)
  pal <- colorNumeric("Purples", domain = bg_data$overlap)

  # Create the map
  map <- leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(
      -86.7667415602124, 36.188530779087586,
      zoom = 5
    ) %>%
    addPolygons(
      data = bg_data,
      fillColor = ~pal(bg_data$overlap),
      fillOpacity = 1,
      weight = 0.5,
      smoothFactor = 0.2,
      stroke = TRUE,
      color = "black",
      popup = ~paste("Block Group GEOID name and number:", "\n", bg_data$NAMELSAD, "\n", bg_data$GEOID)
    ) %>%
    addPolygons(
      data = isochrones_data,
      fill = TRUE,
      stroke = TRUE,
      fillColor = "yellow",
      fillOpacity = 0.05,
      color = "red",
      weight = 1.5
    ) %>%
    addLegend(
      pal = pal,
      values = bg_data$overlap,
      position = "bottomright",
      title = "Intersection with isochrones",
      opacity = 1
    )

  # Export the map to HTML
  saveWidget(map, file = output_html, selfcontained = FALSE)

  # Export the map as a PNG image
  webshot(output_html, output_png)
}
