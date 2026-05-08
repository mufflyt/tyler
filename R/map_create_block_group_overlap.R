#' Function to create and export a map showing block group overlap with isochrones
#'
#' This function creates a map that displays block groups and their overlap with isochrones.
#' The map is exported as an HTML file and a PNG image.
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
#' @importFrom dplyr arrange mutate
#' @importFrom sf st_make_valid st_transform st_is_valid st_union st_sf
#' @importFrom rlang .data
#'
#' @family mapping
#' @export
map_create_block_group_overlap <- function(bg_data, isochrones_data, output_dir = "figures/") {
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("Package 'leaflet' is required for map_create_block_group_overlap(). Install with: install.packages('leaflet')", call. = FALSE)
  }
  if (!requireNamespace("lwgeom", quietly = TRUE)) {
    stop("Package 'lwgeom' is required for map_create_block_group_overlap(). Install with: install.packages('lwgeom')", call. = FALSE)
  }
  if (!requireNamespace("webshot", quietly = TRUE)) {
    stop("Package 'webshot' is required for map_create_block_group_overlap(). Install with: install.packages('webshot')", call. = FALSE)
  }
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop("Package 'htmlwidgets' is required for this function. Install with: install.packages('htmlwidgets')", call. = FALSE)
  }
  if (!inherits(bg_data, "sf")) {
    stop("`bg_data` must be an sf object with polygon geometries.", call. = FALSE)
  }
  if (!inherits(isochrones_data, "sf")) {
    stop("`isochrones_data` must be an sf object with polygon geometries.", call. = FALSE)
  }

  validated <- validate_sf_inputs(
    bg_data = bg_data,
    isochrones_data = isochrones_data,
    expected_types = list(
      bg_data = c("POLYGON", "MULTIPOLYGON"),
      isochrones_data = c("POLYGON", "MULTIPOLYGON")
    ),
    target_crs = 4326,
    context = "map_create_block_group_overlap()"
  )
  bg_data <- validated$bg_data
  isochrones_data <- validated$isochrones_data

  if (!"drive_time" %in% names(isochrones_data)) {
    stop("`isochrones_data` must include a `drive_time` column in minutes.", call. = FALSE)
  }
  if (!is.numeric(isochrones_data$drive_time)) {
    stop("`isochrones_data$drive_time` must be a numeric column.", call. = FALSE)
  }
  if (!"overlap" %in% names(bg_data)) {
    stop("`bg_data` must include an `overlap` column (proportion 0-1). Run calculate_intersection_overlap_and_save() first.", call. = FALSE)
  }
  if (any(!is.na(bg_data$overlap) & (bg_data$overlap < 0 | bg_data$overlap > 1))) {
    stop("`bg_data$overlap` values must be between 0 and 1.", call. = FALSE)
  }

  bg_data <- lwgeom::st_force_polygon_cw(bg_data)
  isochrones_data <- lwgeom::st_force_polygon_cw(isochrones_data)

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

  invisible(lapply(draw_order, function(dt) {
    subset <- isochrones_ordered[isochrones_ordered$drive_time == dt, , drop = FALSE]
    if (!nrow(subset)) {
      return(NULL)
    }

    union_geom <- sf::st_union(subset)
    union_sf <- sf::st_sf(drive_time = dt, geometry = union_geom, crs = 4326)
    union_sf <- lwgeom::st_force_polygon_cw(union_sf)

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
  }))

  # Generate a timestamp
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

  # Define file names with timestamps
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  html_file <- file.path(output_dir, paste0("overlap_bg_map_", timestamp, ".html"))
  png_file <- file.path(output_dir, paste0("overlap_bg_map_", timestamp, ".png"))

  # Export the map to HTML
  htmlwidgets::saveWidget(widget = map, file = html_file, selfcontained = FALSE)
  cat("Map saved as HTML:", html_file, "\n")

  # Export the map as a PNG image
  webshot::webshot(html_file, file = png_file)
  cat("Map saved as PNG:", png_file, "\n")
}
