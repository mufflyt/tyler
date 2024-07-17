#' Get Hospital Referral Region Shapefile
#'
#' This function loads the hospital referral region shapefile and optionally removes Hawaii and Alaska.
#'
#' @param remove_HI_AK Logical, should Hawaii and Alaska be removed? Default is TRUE.
#' @return An sf object containing the hospital referral region data.
#' @importFrom sf read_sf st_transform
#' @importFrom dplyr filter
#'
#' @export
hrr <- function(remove_HI_AK = TRUE) {
  cat("Loading necessary packages...\n")

  # Load the hospital referral region shapefile
  cat("Getting the hospital referral region shapefile...\n")
  hrr <- sf::read_sf("data/hrr-shapefile/Hrr98Bdry_AK_HI_unmodified.shp")
  hrr <- sf::st_transform(hrr, 4326)

  # Optionally remove Hawaii and Alaska
  if (remove_HI_AK) {
    hrr <- hrr %>%
      dplyr::filter(!(hrrcity %in% c("AK- ANCHORAGE", "HI- HONOLULU")))
  }

  # Provide information about the function's purpose
  cat("Hospital Referral Region shapefile loaded.\n")
  cat("This function creates an sf file of hospital referral regions.\n")
  cat("For more information: https://data.dartmouthatlas.org/supplemental/...\n")

  return(hrr)
}

#' Generate Hexagon Maps for Hospital Referral Regions (HRR)
#'
#' This function generates hexagon maps for hospital referral regions.
#'
#' @param physician_sf An sf object containing physician data with coordinates.
#' @param trait_map A string specifying the trait map (default is "all").
#' @param honey_map A string specifying the honey map (default is "all").
#' @return A ggplot object of the generated map.
#' @importFrom sf sf_use_s2 st_transform st_make_grid st_sf st_intersection st_join
#' @importFrom dplyr mutate group_by summarize filter
#' @importFrom ggplot2 geom_sf scale_fill_viridis_c guide_colorbar element_text theme_minimal theme
#' @importFrom ggspatial annotation_scale annotation_north_arrow
#' @importFrom rnaturalearth ne_countries
#'
#' @export
hrr_generate_maps <- function(physician_sf, trait_map = "all", honey_map = "all") {
  sf::sf_use_s2(FALSE)

  # Load USA shapefile
  cat("Loading USA shapefile...\n")
  usa <- rnaturalearth::ne_countries(country = "United States of America", returnclass = "sf")
  usa <- sf::st_transform(usa, 4326)

  # Generate the HRR map
  hrr_map <- hrr()

  # Intersect honeycomb grid with physician data to get physician counts
  cat("Intersecting honeycomb grid with physician data...\n")
  honeycomb_grid_sf <- sf::st_make_grid(usa, c(0.3, 0.3), what = "polygons", square = FALSE) %>%
    sf::st_sf() %>%
    dplyr::mutate(grid_id = row_number()) %>%
    sf::st_intersection(sf::st_transform(usa, 4326))

  intersections <- sf::st_intersection(honeycomb_grid_sf, sf::st_transform(physician_sf, 4326)) %>%
    dplyr::filter(grid_id > 9546L)  # Filter out Palmyra Atoll

  physician_count_per_honey <- intersections %>%
    dplyr::group_by(grid_id) %>%
    dplyr::summarize(physician_count = n(), .groups = 'drop') %>%
    dplyr::filter(physician_count > 1)

  # Join honeycomb grid with physician count
  honeycomb_grid_with_physicians <- sf::st_join(honeycomb_grid_sf, physician_count_per_honey) %>%
    dplyr::filter(!is.na(physician_count))

  # Create the map with honeycomb and HRR map using ggplot2
  cat("Creating the map...\n")
  map_ggplot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = hrr_map, fill = "#D3D3D3", color = "darkblue", size = 1.5) +
    ggplot2::geom_sf(data = honeycomb_grid_with_physicians, aes(fill = physician_count), color = NA) +
    ggplot2::scale_fill_viridis_c(
      breaks = c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150),
      name = "Physician Count",
      guide = ggplot2::guide_colorbar(
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5,
        label.hjust = 0.5,
        barwidth = 10,
        barheight = 1,
        alpha = 0.7,
        label.theme = ggplot2::element_text(size = 8, color = "black", hjust = 0.5)
      )) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(
        fill = "floralwhite",
        color = "darkblue",
        size = 1,
        linetype = "solid"
      ),
      axis.text = ggplot2::element_text(size = 4, color = "darkgray", family = "Helvetica"),
      axis.title = ggplot2::element_text(size = 12, face = "bold", family = "Helvetica"),
      plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5, family = "Helvetica"),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold", color = "darkblue", size = 10, family = "Helvetica"),
      legend.text = ggplot2::element_text(size = 8, family = "Helvetica"),
      legend.background = ggplot2::element_rect(
        fill = "aliceblue",
        color = "black",
        size = 0.5
      ),
      plot.margin = ggplot2::margin(10, 10, 10, 10),
      legend.key = ggplot2::element_rect(fill = "aliceblue", color = "black", size = 0.5)
    ) +
    ggspatial::annotation_scale(location = "bl", width_hint = 0.5, bar_cols = c("black", "white")) +
    ggspatial::annotation_north_arrow(location = "br", which_north = "true", style = ggspatial::north_arrow_fancy_orienteering()) +
    ggplot2::theme(panel.grid.major = ggplot2::element_line(color = "lightgray")) +
    ggplot2::theme(panel.border = ggplot2::element_rect(color = "darkblue", fill = NA, size = 1.5))

  # Print the plot
  print(map_ggplot)

  # Save the map in various formats
  cat("Saving the map...\n")
  ggplot2::ggsave(filename = paste0("figures/hexmap/hexmap_figures/", trait_map, "_", honey_map, "_honey.tiff"), plot = map_ggplot, width = 10, height = 6, dpi = 800)

  return(map_ggplot)
}
