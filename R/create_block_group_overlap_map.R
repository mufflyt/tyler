#' Function to create and export a map showing block group overlap with isochrones
#'
#' This function creates a map that displays block groups and their overlap with isochrones.
# The map is exported as an HTML file and a PNG image.
#
#' @param bg_data A SpatialPolygonsDataFrame representing block group data.
#' @param isochrones_data A SpatialPolygonsDataFrame representing isochrone data.
#' @param output_dir Directory path for exporting the map files. Default is "figures/".
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' # Create and export the map with the default output directory
#' create_block_group_overlap_map(block_groups, isochrones_joined_map)
#'
#' # Create and export the map with a custom output directory
#' create_block_group_overlap_map(block_groups, isochrones_joined_map, "custom_output/")
#' }
#'
#' @importFrom leaflet addPolygons addLegend colorNumeric
#' @importFrom webshot webshot
#' @importFrom htmlwidgets saveWidget
#' @importFrom sf st_transform
#'
#' @export
create_block_group_overlap_map <- function(bg_data, isochrones_data, output_dir = "figures/") {
  bg_data <- sf::st_transform(bg_data, 4326)
  pal <- leaflet::colorNumeric("Purples", domain = bg_data$overlap)

  # Create the base map
  base_map <- tyler::createBaseMap("<h1>Block Group Overlap Map</h1>")

  # Create the map
  map <- base_map %>%
    leaflet::addPolygons(
      data = bg_data,
      fillColor = ~pal(bg_data$overlap),
      fillOpacity = 1,
      weight = 0.5,
      smoothFactor = 0.2,
      stroke = TRUE,
      color = "black",
      popup = ~paste("Block Group GEOID name and number:", "\n", bg_data$NAMELSAD, "\n", bg_data$GEOID)
    ) %>%
    leaflet::addPolygons(
      data = isochrones_data,
      fill = TRUE,
      stroke = TRUE,
      fillColor = "yellow",
      fillOpacity = 0.05,
      color = "red",
      weight = 1.5
    ) %>%
    leaflet::addLegend(
      pal = pal,
      values = bg_data$overlap,
      position = "bottomright",
      title = "Intersection with isochrones",
      opacity = 1
    )

  # Generate a timestamp
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

  # Define file names with timestamps
  html_file <- paste0(output_dir, "overlap_bg_map_", timestamp, ".html")
  png_file <- paste0(output_dir, "overlap_bg_map_", timestamp, ".png")

  # Export the map to HTML
  htmlwidgets::saveWidget(widget = map, file = html_file, selfcontained = FALSE)
  cat("Map saved as HTML:", html_file, "\n")

  # Export the map as a PNG image
  webshot::webshot(html_file, file = png_file)
  cat("Map saved as PNG:", png_file, "\n")
}
