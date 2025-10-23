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
#' @examples
#' hrr()
hrr <- function(remove_HI_AK = TRUE) {
  cat("Loading necessary packages...\n")

  # Load the hospital referral region shapefile
  cat("Getting the hospital referral region shapefile...\n")
  hrr_path <- system.file(
    "extdata",
    "hrr-shapefile/Hrr98Bdry_AK_HI_unmodified.shp",
    package = "tyler"
  )
  hrr <- sf::read_sf(hrr_path)
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
#' @param dpi Resolution used when saving the final figure (default is 600).
#' @param width Final figure width in inches for journal submission (default is 7).
#' @param height Final figure height in inches for journal submission (default is 5).
#' @return Invisibly returns the ggplot object of the generated map.
#' @importFrom sf sf_use_s2 st_transform st_make_grid st_sf st_intersection st_join st_filter
#' @importFrom dplyr mutate group_by summarize filter
#' @importFrom ggplot2 geom_sf scale_fill_viridis_c guide_colorbar element_text theme_minimal theme labs
#' @importFrom ggspatial annotation_scale annotation_north_arrow
#' @importFrom rnaturalearth ne_countries
#' @importFrom stringr str_detect
#' @importFrom scales pretty_breaks label_number squish
#' @importFrom gridExtra arrangeGrob
#' @importFrom grid grid.newpage grid.draw
#' @importFrom fs dir_create path
#'
#' @export
#' @examples
#' \dontrun{
#' hrr_generate_maps(physician_sf)
#' }
hrr_generate_maps <- function(
    physician_sf,
    trait_map = "all",
    honey_map = "all",
    dpi = 600,
    width = 7,
    height = 5
) {
  sf::sf_use_s2(FALSE)

  # Load USA shapefile
  cat("Loading USA shapefile...\n")
  usa <- rnaturalearth::ne_countries(country = "United States of America", returnclass = "sf")
  usa <- sf::st_transform(usa, 4326)

  # Generate the HRR map (retain AK, HI, and PR for insets)
  hrr_map <- hrr(remove_HI_AK = FALSE)

  # Intersect honeycomb grid with physician data to get physician counts
  cat("Intersecting honeycomb grid with physician data...\n")
  honeycomb_grid_sf <- sf::st_make_grid(usa, c(0.3, 0.3), what = "polygons", square = FALSE) %>%
    sf::st_sf() %>%
    dplyr::mutate(grid_id = dplyr::row_number()) %>%
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

  fill_limits <- range(honeycomb_grid_with_physicians$physician_count, na.rm = TRUE)
  if (any(is.infinite(fill_limits))) {
    fill_limits <- c(0, 1)
  }

  pretty_breaks <- scales::pretty_breaks(n = 6)(fill_limits)

  # Split data for contiguous US and insets
  cat("Preparing contiguous and inset geographies...\n")
  contiguous_hrr <- hrr_map %>%
    dplyr::filter(!stringr::str_detect(hrrcity, "^(AK|HI|PR)-"))
  alaska_hrr <- hrr_map %>% dplyr::filter(stringr::str_detect(hrrcity, "^AK-"))
  hawaii_hrr <- hrr_map %>% dplyr::filter(stringr::str_detect(hrrcity, "^HI-"))
  puerto_rico_hrr <- hrr_map %>% dplyr::filter(stringr::str_detect(hrrcity, "^PR-"))

  contiguous_honey <- sf::st_filter(honeycomb_grid_with_physicians, contiguous_hrr)
  alaska_honey <- sf::st_filter(honeycomb_grid_with_physicians, alaska_hrr)
  hawaii_honey <- sf::st_filter(honeycomb_grid_with_physicians, hawaii_hrr)
  puerto_rico_honey <- sf::st_filter(honeycomb_grid_with_physicians, puerto_rico_hrr)

  map_theme <- ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      axis.title = ggplot2::element_text(face = "bold"),
      legend.title = ggplot2::element_text(face = "bold"),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.margin = ggplot2::margin(t = 6)
    )

  build_region_plot <- function(hrr_data, honey_data, region_title, show_legend = FALSE, base_size = 10) {
    if (nrow(hrr_data) == 0) {
      return(
        ggplot2::ggplot() +
          ggplot2::theme_void() +
          ggplot2::annotate("text", x = 0.5, y = 0.5, label = paste("No data for", region_title), size = 3)
      )
    }

    ggplot2::ggplot() +
      ggplot2::geom_sf(data = hrr_data, fill = "#f5f5f5", colour = "#4a4a4a", linewidth = 0.2) +
      ggplot2::geom_sf(
        data = honey_data,
        ggplot2::aes(fill = physician_count),
        colour = NA,
        alpha = 0.95
      ) +
      ggplot2::scale_fill_viridis_c(
        option = "viridis",
        limits = fill_limits,
        oob = scales::squish,
        breaks = pretty_breaks,
        labels = scales::label_number(accuracy = 1),
        name = "Physician count"
      ) +
      ggplot2::labs(title = region_title) +
      ggplot2::theme_minimal(base_size = base_size) +
      ggplot2::theme(
        axis.title = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank(),
        legend.position = if (show_legend) "bottom" else "none",
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
      )
  }

  cat("Creating main map and insets...\n")
  main_map <- build_region_plot(contiguous_hrr, contiguous_honey, "Hospital Referral Regions", show_legend = TRUE, base_size = 12) +
    ggspatial::annotation_scale(location = "bl", width_hint = 0.25, bar_cols = c("black", "white")) +
    ggspatial::annotation_north_arrow(
      location = "br",
      which_north = "true",
      style = ggspatial::north_arrow_fancy_orienteering()
    ) +
    map_theme

  alaska_inset <- build_region_plot(alaska_hrr, alaska_honey, "Alaska")
  hawaii_inset <- build_region_plot(hawaii_hrr, hawaii_honey, "Hawaii")
  puerto_rico_inset <- build_region_plot(puerto_rico_hrr, puerto_rico_honey, "Puerto Rico")

  combined_insets <- gridExtra::arrangeGrob(alaska_inset, hawaii_inset, puerto_rico_inset, ncol = 3)
  combined_map <- gridExtra::arrangeGrob(main_map, combined_insets, heights = c(3, 1))

  # Print the plot
  grid::grid.newpage()
  grid::grid.draw(combined_map)

  # Ensure output directory exists
  output_dir <- fs::path("figures", "hexmap", "hexmap_figures")
  fs::dir_create(output_dir)

  output_stub <- fs::path(output_dir, paste0(trait_map, "_", honey_map, "_honey"))
  cat("Saving the map for Obstetrics & Gynecology submission...\n")
  ggplot2::ggsave(paste0(output_stub, ".tiff"), plot = combined_map, width = width, height = height, dpi = dpi, units = "in", compression = "lzw")
  ggplot2::ggsave(paste0(output_stub, ".png"), plot = combined_map, width = width, height = height, dpi = dpi, units = "in")

  invisible(combined_map)
}
