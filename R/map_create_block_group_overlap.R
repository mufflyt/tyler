#' Function to create and export a map showing block group overlap with isochrones
#'
#' This function creates a map that displays block groups and their overlap with isochrones.
#' The map is exported as an HTML file and a PNG image.
#' @param bg_data A SpatialPolygonsDataFrame representing block group data.
#' @param isochrones_data A SpatialPolygonsDataFrame representing isochrone data.
#' @param output_dir Directory path for exporting the map files. Default is "figures/".
#'
#' @return Called for its side effects of saving the block group overlap map as HTML and PNG inside `output_dir`. Returns `NULL` invisibly.
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
#' @importFrom sf st_make_valid st_transform st_is_valid st_union st_sf
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
  checkmate::assert_string(output_dir, min.chars = 1, .var.name = "output_dir")
  checkmate::assert_class(bg_data, classes = "sf", .var.name = "bg_data")
  checkmate::assert_class(isochrones_data, classes = "sf", .var.name = "isochrones_data")
  checkmate::assert_true(nrow(bg_data) > 0, .var.name = "bg_data")
  checkmate::assert_true(nrow(isochrones_data) > 0, .var.name = "isochrones_data")
  checkmate::assert_names(names(isochrones_data), must.include = "drive_time", .var.name = "names(isochrones_data)")
  checkmate::assert_names(names(bg_data), must.include = c("overlap", "NAMELSAD", "GEOID"), .var.name = "names(bg_data)")
  checkmate::assert_numeric(isochrones_data$drive_time, any.missing = FALSE, finite = TRUE, lower = 0, .var.name = "isochrones_data$drive_time")
  checkmate::assert_numeric(bg_data$overlap, any.missing = FALSE, finite = TRUE, lower = 0, upper = 1, .var.name = "bg_data$overlap")
  checkmate::assert_true(any(isochrones_data$drive_time %in% c(30, 60, 120, 180)),
    .var.name = "isochrones_data$drive_time"
  )

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

  bg_data <- lwgeom::st_force_polygon_cw(bg_data)
  isochrones_data <- lwgeom::st_force_polygon_cw(isochrones_data)

  palette <- c(
    "180" = "#ff0000",
    "120" = "#ffd700",
    "60" = "#228b22",
    "30" = "#1f77b4"
  )

  draw_order <- c(180, 120, 60, 30)

  isochrones_ordered <- isochrones_data
  isochrones_ordered[["drive_time"]] <- as.numeric(isochrones_ordered[["drive_time"]])
  isochrones_ordered <- isochrones_ordered[order(match(isochrones_ordered[["drive_time"]], draw_order)), , drop = FALSE]

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
  message("Map saved as HTML: ", html_file)

  # Export the map as a PNG image
  webshot::webshot(html_file, file = png_file)
  message("Map saved as PNG: ", png_file)
}
