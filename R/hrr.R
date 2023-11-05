#' Get Hospital Referral Region Shapefile
#'
#' This function loads the hospital referral region shapefile and optionally removes Hawaii and Alaska.
#'
#' @param remove_HI_AK Logical, should Hawaii and Alaska be removed?
#' @return A SpatialPolygonsDataFrame containing the hospital referral region data.
#' @import sf
#' @import dplyr
#'
#' @export

hrr <- function(remove_HI_AK = TRUE) {
  # Load necessary packages
  cat("Loading necessary packages...\n")

  # Get the hospital referral region shapefile
  cat("Getting the hospital referral region shapefile...\n")
  hrr <- sf::read_sf("data/hrr-shapefile/Hrr98Bdry_AK_HI_unmodified.shp")
  hrr <- sf::st_transform(hrr, 4326)

  # Optionally remove Hawaii and Alaska
  if (remove_HI_AK) {
    hrr <- hrr %>%
      dplyr::filter(!(hrrcity %in% c("AK- ANCHORAGE", "HI- HONOLULU")))
  }

  # Plot the first record from the shapefile
  plot(hrr[1])

  # Provide information about the function's purpose
  cat("Hospital Referral Region shapefile creation complete...\n")
  cat("This function creates a plottable sf file of hospital referral regions.\n")
  cat("For reference and more information: https://data.dartmouthatlas.org/supplemental/...\n")

  return(hrr)
}

#hrr(remove_HI_AK = TRUE)
# Use case:
#hrr(remove_HI_AK = TRUE)


####
hrr_generate_maps <- function(physician_sf, trait_map = "all", honey_map = "all", breaks = c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200), map_title = "OBGYN Residency\n Faculty Subspecialists") {
  # Disabling the use of the s2 geometry library
  cat("Install all needed packages...\n")
  sf::sf_use_s2(FALSE)

  # Load physician data
  cat("Loading physician data from an RDS file...  Include columns named long and lat...\n")
  physicians <- readRDS("data/Physicians.rds")
  physicians <- physicians[!is.na(physicians$long), ]
  physician_sf <- sf::st_as_sf(physicians, coords = c("long", "lat"), crs = 4326)

  # Load USA shapefile
  cat("Loading USA shapefile...\n")
  usa <- rnaturalearth::ne_countries(country = "United States of America", returnclass = "sf")
  usa <- sf::st_transform(usa, 4326)

  cat("Starting generate_maps function...\n")

  # Create the directory if not exists
  cat("Checking directory...\n")
  if (!dir.exists("figures/hexmap/hexmap_figures")) {
    dir.create("figures/hexmap/hexmap_figures", recursive = TRUE)
  }

  # Generate the HRR map
  hrr_map <- tyler::hrr()

  # Intersect honeycomb grid with physician data to get physician counts
  cat("Intersecting honeycomb grid with physician data...\n")
  honeycomb_grid_sf <- sf::st_make_grid(usa, c(0.3, 0.3), what = "polygons", square = FALSE) %>%
    sf::st_sf() %>%
    dplyr::mutate(grid_id = row_number()) %>%
    sf::st_intersection(sf::st_transform(usa, 4326))

  intersections <- sf::st_intersection(honeycomb_grid_sf, sf::st_transform(physician_sf, 4326)) %>%
    dplyr::filter(grid_id > 9546L)
  cat("Filtered out Palmyra Atoll by setting grid_id to greater than 9546.... \n")

  physician_count_per_honey <- intersections %>%
    dplyr::group_by(grid_id) %>%
    dplyr::summarize(physician_count = n(), .groups = 'drop') %>%
    dplyr::filter(physician_count > 1)

  # Join honeycomb grid with physician count
  honeycomb_grid_with_physicians <- sf::st_join(honeycomb_grid_sf, physician_count_per_honey) %>%
    dplyr::filter(!is.na(physician_count))

  # Check for missing values in the physician_count column
  any(is.na(honeycomb_grid_with_physicians$physician_count))

  # Check summary statistics of physician_count
  summary(honeycomb_grid_with_physicians$physician_count)

  # Create the map with honeycomb and HRR map using ggplot2
  cat("Creating the map...\n")

  # Filter out Alaska by specifying a condition
  map_ggplot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = hrr_map, fill = "#D3D3D3", color = "darkblue", size = 1.5) +
    ggplot2::geom_sf(data = honeycomb_grid_with_physicians, aes(fill = physician_count), color = NA) +
    ggplot2::scale_fill_continuous(name = "Physician Count", trans = "log10") +  # Apply the log scale
    ggplot2::theme_minimal(base_size = 10) +
    # ggplot() +
    # geom_sf(data = hrr_map, fill = "#D3D3D3", color = "darkblue", size = 1.5) +
    # geom_sf(data = honeycomb_grid_with_physicians, aes(fill = physician_count), color = NA) +
    # scale_fill_viridis_c(
    #   breaks = c(1, 5, 10, 20, 30, 40, 60, 80, 100, 150),
    #   name = "Obstetrics and Gynecology Faculty Subspecialists",
    #   guide = ggplot2::guide_colorbar(
    #     direction = "horizontal",
    #     title.position = "top",
    #     title.hjust = 0.5,
    #     label.hjust = 0.5,
    #     barwidth = 10,
    #     barheight = 1,
    #     alpha = 0.7,
    #     label.theme = element_text(size = 8, color = "black", hjust = 0.5)
    #   )) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(
      panel.background = element_rect(
        fill = "floralwhite",
        color = "darkblue",
        size = 1,
        linetype = "solid"
      ),
      axis.text = element_text(size = 4, color = "darkgray", family = "Helvetica"),
      axis.title = element_text(size = 12, face = "bold", family = "Helvetica"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5, family = "Helvetica"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", color = "darkblue", size = 10, family = "Helvetica"),
      legend.text = element_text(size = 8, family = "Helvetica"),
      legend.background = element_rect(
        fill = "aliceblue",
        color = "black",
        size = 0.5
      ),
      plot.margin = margin(10, 10, 10, 10),
      legend.key = element_rect(fill = "aliceblue", color = "black", size = 0.5)
    ) +
    ggspatial::annotation_scale(location = "bl", width_hint = 0.5, bar_cols = c("black", "white")) +
    ggspatial::annotation_north_arrow(location = "br", which_north = "true", style = north_arrow_fancy_orienteering()) +
    ggplot2::theme(panel.grid.major = element_line(color = "lightgray")) +
    ggplot2::theme(panel.border = element_rect(color = "darkblue", fill = NA, size = 1.5))

  # Print the plot
  print(map_ggplot)

  # Save the map in various formats
  cat("Saving the map...\n")
  ggplot2::ggsave(filename = paste0("figures/hexmap/hexmap_figures/", trait_map, "_", honey_map, "_honey.tiff"), plot = map_ggplot, width = 10, height = 6, dpi = 800)

  return(map_ggplot)
}

#### Case example:
  # hrr_generate_maps(
  #   trait_map = "all",
  #   honey_map = "all")


###############################################
# hsa: Hospital service areas

hsa <- function(remove_HI_AK = TRUE) {
  # Get the hospital referral region shapefile
  cat("Getting the hospital service areas shapefile...\n")
  hsa <- sf::read_sf("data/hsa-shapefile/HsaBdry_AK_HI_unmodified.shp")
  hsa <- sf::st_transform(hsa, 4326)

  # Plot the first record from the shapefile
  plot(hsa[1])

  # Provide information about the function's purpose
  cat("Hospital Service Area shapefile creation complete.\n")
  cat("This function creates a plottable sf file of hospital service area.\n")
  cat("For reference and more information: https://data.dartmouthatlas.org/supplemental/....\n")

  return(hsa)
}

  hsa_generate_maps <- function(physician_sf, trait_map = "all", honey_map = "all", breaks = c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200), map_title = "OBGYN Residency\n Faculty Subspecialists") {

    sf::sf_use_s2(FALSE)

    # Load physician data
    cat("Loading physician data from an RDS file...  Include columns named long and lat...\n")
    physicians <- readRDS("data/Physicians.rds")
    physicians <- physicians[!is.na(physicians$long), ]
    physician_sf <- sf::st_as_sf(physicians, coords = c("long", "lat"), crs = 4326)

    # Load USA shapefile
    cat("Loading USA shapefile...\n")
    usa <- rnaturalearth::ne_countries(country = "United States of America", returnclass = "sf")
    usa <- sf::st_transform(usa, 4326)

    cat("Starting generate_maps function...\n")

    # Create the directory if not exists
    cat("Checking directory...\n")
    if (!dir.exists("figures/hexmap/hexmap_figures")) {
      dir.create("figures/hexmap/hexmap_figures", recursive = TRUE)
    }

    # Generate the HRR map
    hsa_map <- hsa()

    # Intersect honeycomb grid with physician data to get physician counts
    cat("Intersecting honeycomb grid with physician data...\n")
    honeycomb_grid_sf <- sf::st_make_grid(usa, c(0.3, 0.3), what = "polygons", square = FALSE) %>%
      sf::st_sf() %>%
      dplyr::mutate(grid_id = row_number()) %>%
      sf::st_intersection(sf::st_transform(usa, 4326))

    intersections <- sf::st_intersection(honeycomb_grid_sf, sf::st_transform(physician_sf, 4326)) %>%
      dplyr::filter(grid_id > 9546L)
    cat("Filtered out Palmyra Atoll by setting grid_id to greater than 9546.... \n")

    physician_count_per_honey <- intersections %>%
      dplyr::group_by(grid_id) %>%
      dplyr::summarize(physician_count = n(), .groups = 'drop') %>%
      dplyr::filter(physician_count > 1)

    # Join honeycomb grid with physician count
    honeycomb_grid_with_physicians <- sf::st_join(honeycomb_grid_sf, physician_count_per_honey) %>%
      dplyr::filter(!is.na(physician_count))

    # Check for missing values in the physician_count column
    any(is.na(honeycomb_grid_with_physicians$physician_count))

    # Check summary statistics of physician_count
    summary(honeycomb_grid_with_physicians$physician_count)

    # Create the map with honeycomb and hsa map using ggplot2
    cat("Creating the map...\n")

    # Filter out Alaska by specifying a condition
    map_ggplot <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = hsa_map, fill = "#D3D3D3", color = "darkblue", size = 1.5) +
      ggplot2::geom_sf(data = honeycomb_grid_with_physicians, aes(fill = physician_count), color = NA) +
      ggplot2::scale_fill_viridis_c(
        breaks = c(1, 5, 10, 20, 30, 40, 60, 80, 100, 150),
        name = "Obstetrics and Gynecology Faculty Subspecialists",
        guide = ggplot2::guide_colorbar(
          direction = "horizontal",
          title.position = "top",
          title.hjust = 0.5,
          label.hjust = 0.5,
          barwidth = 10,
          barheight = 1,
          alpha = 0.7,
          label.theme = element_text(size = 8, color = "black", hjust = 0.5)
        )) +
      ggplot2::theme_minimal(base_size = 10) +
      ggplot2::theme(
        panel.background = element_rect(
          fill = "floralwhite",
          color = "darkblue",
          size = 1,
          linetype = "solid"
        ),
        axis.text = element_text(size = 4, color = "darkgray", family = "Helvetica"),
        axis.title = element_text(size = 12, face = "bold", family = "Helvetica"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5, family = "Helvetica"),
        legend.position = "bottom",
        legend.title = element_text(face = "bold", color = "darkblue", size = 10, family = "Helvetica"),
        legend.text = element_text(size = 8, family = "Helvetica"),
        legend.background = element_rect(
          fill = "aliceblue",
          color = "black",
          size = 0.5
        ),
        plot.margin = margin(10, 10, 10, 10),
        legend.key = element_rect(fill = "aliceblue", color = "black", size = 0.5)
      ) +
      ggspatial::annotation_scale(location = "bl", width_hint = 0.5, bar_cols = c("black", "white")) +
      ggspatial::annotation_north_arrow(location = "br", which_north = "true", style = north_arrow_fancy_orienteering()) +
      ggplot2::theme(panel.grid.major = element_line(color = "lightgray")) +
      ggplot2::theme(panel.border = element_rect(color = "darkblue", fill = NA, size = 1.5))

    # Print the plot
    print(map_ggplot)

    # Save the map in various formats
    cat("Saving the map...\n")
    ggplot2::ggsave(filename = paste0("figures/hexmap/hexmap_figures/", trait_map, "_", honey_map, "_honey.tiff"), plot = map_ggplot, width = 10, height = 6, dpi = 800)

    return(map_ggplot)
  }

  #### Case example:
  # hsa_generate_maps(
  #   trait_map = "all",
  #   honey_map = "all")
