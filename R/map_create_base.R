#' Create a Configurable Leaflet Base Map
#'
#' Build a Leaflet base map with sensible defaults for the tyler mapping
#' helpers. The map includes multiple tile providers, a scale bar, optional
#' title control, and centres on the continental United States by default.
#'
#' @param title Optional HTML string used for a title control in the upper
#'   left corner of the map. Supply `NULL` or an empty string to omit the
#'   control.
#' @param lat,lng Numeric latitude and longitude used to centre the initial
#'   view. Defaults position the map over the continental United States.
#' @param zoom Numeric zoom level passed to [leaflet::setView()].
#'
#' @return A [leaflet::leaflet()] map object pre-configured with controls and
#'   basemap layers.
#'
#' @family mapping
#' @export
#' @examples
#' map_create_base()
#' map_create_base("<strong>Custom title</strong>")
map_create_base <- function(title = NULL, lat = 39.8282, lng = -98.5795, zoom = 4) {
  map <- leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = TRUE)) %>%
    leaflet::addProviderTiles("CartoDB.Voyager", group = "CartoDB Voyager") %>%
    leaflet::addProviderTiles("Stamen.TonerLite", group = "Toner by Stamen") %>%
    leaflet::addScaleBar(position = "bottomleft") %>%
    leaflet::addLayersControl(
      baseGroups = c("CartoDB Voyager", "Toner by Stamen"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) %>%
    leaflet::setView(lat = lat, lng = lng, zoom = zoom) %>%
    leaflet::addTiles(options = leaflet::tileOptions(useCache = TRUE, crossOrigin = TRUE))

  if (!is.null(title) && nzchar(title)) {
    map <- leaflet::addControl(
      map,
      html = htmltools::tags$div(
        class = "tyler-map-title",
        htmltools::HTML(title)
      ),
      position = "topleft"
    )
  }

  map
}

#' Create and Save a Leaflet Dot Map of Physicians
#'
#' This function creates a Leaflet dot map of physicians using their longitude
#' and latitude coordinates. It also adds ACOG district boundaries to the map
#' and saves it as an HTML file with an accompanying PNG screenshot.
#'
#' @param physician_data An sf object containing physician data with `"long"`
#'   and `"lat"` columns.
#' @param jitter_range The range for adding jitter to latitude and longitude
#'   coordinates.
#' @param color_palette The color palette for ACOG district colors.
#' @param popup_var The variable to use for popup text.
#' @return Invisibly returns the Leaflet map object.
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
#' map_create_physician_dot(physician_data)
#' }
#'
#' @family mapping
#' @export
map_create_physician_dot <- function(physician_data, jitter_range = 0.05, color_palette = "magma", popup_var = "name") {
  jittered_physician_data <- dplyr::mutate(
    physician_data,
    lat = lat + runif(n()) * jitter_range,
    long = long + runif(n()) * jitter_range
  )

  cat("Setting up the base map...\n")
  base_map <- map_create_base("Physician Dot Map")
  cat("Map setup complete.\n")

  cat("Generating the ACOG district boundaries...\n")
  acog_districts <- map_create_acog_districts_sf()
  cat("ACOG district boundaries generated.\n")

  num_acog_districts <- dplyr::n_distinct(acog_districts$ACOG_District)
  district_colors <- viridis::viridis(num_acog_districts, option = color_palette)

  jittered_physician_data <- dplyr::mutate(
    jittered_physician_data,
    ACOG_District = factor(
      ACOG_District,
      levels = sort(unique(acog_districts$ACOG_District))
    )
  )

  dot_map <- leaflet::addCircleMarkers(
    base_map,
    data = jittered_physician_data,
    lng = ~long,
    lat = ~lat,
    radius = 3,
    stroke = TRUE,
    weight = 1,
    color = district_colors[as.numeric(jittered_physician_data$ACOG_District)],
    fillOpacity = 0.8,
    popup = as.formula(paste0("~", popup_var))
  ) %>%
    leaflet::addPolygons(
      data = acog_districts,
      color = "red",
      weight = 2,
      fill = FALSE,
      opacity = 0.8,
      popup = ~ACOG_District
    ) %>%
    leaflet::addLegend(
      position = "bottomright",
      colors = district_colors,
      labels = levels(jittered_physician_data$ACOG_District),
      title = "ACOG Districts"
    )

  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  html_file <- paste0("figures/dot_map_", timestamp, ".html")
  png_file <- paste0("figures/dot_map_", timestamp, ".png")

  htmlwidgets::saveWidget(widget = dot_map, file = html_file, selfcontained = TRUE)
  cat("Leaflet map saved as HTML:", html_file, "\n")

  webshot::webshot(html_file, file = png_file)
  cat("Screenshot saved as PNG:", png_file, "\n")

  invisible(dot_map)
}
