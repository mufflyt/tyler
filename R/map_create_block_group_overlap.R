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
#' map_create_block_group_overlap(block_groups, isochrones_joined_map)
#'
#' # Create and export the map with a custom output directory
#' map_create_block_group_overlap(block_groups, isochrones_joined_map, "custom_output/")
#' }
#'
#' @importFrom leaflet addPolygons addLegend colorNumeric
#' @importFrom webshot webshot
#' @importFrom htmlwidgets saveWidget
#' @importFrom sf st_make_valid st_transform st_is_valid st_union st_sf
#' @importFrom lwgeom st_orient
#' @importFrom dplyr arrange mutate
#' @importFrom purrr walk
#' @importFrom rlang .data
#'
#' @family mapping
#' @export
map_create_block_group_overlap <- function(bg_data, isochrones_data, output_dir = "figures/") {
  if (!inherits(bg_data, "sf")) {
    stop("`bg_data` must be an sf object with polygon geometries.")
  }
  if (!inherits(isochrones_data, "sf")) {
    stop("`isochrones_data` must be an sf object with polygon geometries.")
  }

  bg_data <- sf::st_make_valid(bg_data)
  isochrones_data <- sf::st_make_valid(isochrones_data)

  if (!all(sf::st_is_valid(bg_data))) {
    stop("Block group geometries remain invalid after attempting repair.")
  }
  if (!all(sf::st_is_valid(isochrones_data))) {
    stop("Isochrone geometries remain invalid after attempting repair.")
  }

  bg_data <- sf::st_transform(bg_data, 4326)
  isochrones_data <- sf::st_transform(isochrones_data, 4326)

  bg_data <- lwgeom::st_orient(bg_data)
  isochrones_data <- lwgeom::st_orient(isochrones_data)

  if (!"drive_time" %in% names(isochrones_data)) {
    stop("`isochrones_data` must include a `drive_time` column in minutes.")
  }

  palette <- c(
    "180" = "#ff0000",
    "120" = "#ffd700",
    "60" = "#228b22",
    "30" = "#1f77b4"
  )

  draw_order <- c(180, 120, 60, 30)

  isochrones_ordered <- isochrones_data %>%
    dplyr::mutate(drive_time = as.numeric(rlang::.data$drive_time)) %>%
    dplyr::arrange(match(rlang::.data$drive_time, draw_order))

  pal <- leaflet::colorNumeric("Purples", domain = bg_data$overlap)

  # Create the base map
  base_map <- map_create_base("<h1>Block Group Overlap Map</h1>")

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
    leaflet::addLegend(
      pal = pal,
      values = bg_data$overlap,
      position = "bottomright",
      title = "Intersection with isochrones",
      opacity = 1
    )

  purrr::walk(draw_order, function(dt) {
    subset <- isochrones_ordered[isochrones_ordered$drive_time == dt, , drop = FALSE]
    if (!nrow(subset)) {
      return(NULL)
    }

    union_geom <- sf::st_union(subset)
    union_sf <- sf::st_sf(drive_time = dt, geometry = union_geom, crs = 4326)
    union_sf <- lwgeom::st_orient(union_sf)

    map <<- map %>%
      leaflet::addPolygons(
        data = union_sf,
        fill = TRUE,
        stroke = TRUE,
        fillColor = palette[as.character(dt)],
        fillOpacity = 0.35,
        color = palette[as.character(dt)],
        weight = 1.5,
        group = paste0(dt, " minutes")
      )
  })

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
