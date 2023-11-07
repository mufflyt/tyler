#' Generate ACOG Districts sf Object
#'
#' This function generates an sf object representing the ACOG districts by grouping all the states in a particular district together.
#'
#' @return An sf object containing grouped ACOG districts.
#' @importFrom sf st_transform st_sf
#' @importFrom dplyr left_join group_by summarize
#' @importFrom rnaturalearth ne_states
#' @importFrom sf sf_use_s2
#' @import RColorBrewer
#' @export
generate_acog_districts_sf <- function(filepath = NULL) {
  if (is.null(filepath)) {
    message("No file path provided. Using default data source.")
    filepath <- NULL  # Provide the path to your CSV file if needed
  }

  options(tigris_use_cache = TRUE)
  sf::sf_use_s2(FALSE)

  cat("Using ACOG_Districts dataframe...\n")
  # Use the ACOG_Districts dataframe from the tyler package
  ACOG_Districts <- tyler::ACOG_Districts

  cat("Getting the states shapefile...\n")
  # Get the states shapefile
  states <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
  states <- sf::st_transform(states, 4326)

  cat("Joining ACOG Districts with states and transforming...\n")
  # Join ACOG Districts with states and transform
  states <- states %>%
    dplyr::left_join(ACOG_Districts, by = c("name" = "State")) %>%
    dplyr::mutate(ACOG_District = dplyr::if_else(name == "Alaska" & ACOG_District == "VIII", NA_character_, ACOG_District)) %>%
    sf::st_transform(4326)

  cat("Grouping states by ACOG District and combining...\n")
  # Group states by ACOG District and combine them
  ACOG_Districts_sf <- states %>%
    dplyr::group_by(ACOG_District) %>%
    dplyr::summarize(geometry = sf::st_union(geometry), .groups = 'drop') %>%
    sf::st_sf()

  cat("ACOG Districts sf creation complete. Creates a plottable sf file of ACOG districts.\n")
  cat("Reference: https://www.acog.org/community/districts-and-sections.\n")

  return(ACOG_Districts_sf)
}

#' Generate Hexagon Maps by ACOG District
#'
#' This function generates hexagon maps for ACOG districts.
#'
#' @param physician_sf A sf object containing physician data with coordinates.
#' @param acog_districts_sf A sf object containing the grouped ACOG districts.
#' @param trait_map A string specifying the trait map (default is "all").
#' @param honey_map A string specifying the honey map (default is "all").
#' @param grid_size A numeric vector of length 2 specifying the grid size for the hexagon map (default is c(0.3, 0.3)).
#' @param specific_district A string or NULL to specify a specific district for generating the map (default is NULL, which processes all districts).
#'
#' @return A ggplot object of the generated map for the specified or all districts.
#' @importFrom sf st_transform st_sf sf_use_s2
#' @importFrom dplyr filter group_by summarize
#' @importFrom rnaturalearth ne_countries
#' @importFrom ggplot2 ggplot geom_sf scale_fill_viridis_c theme_light theme labs ggsave
#' @import RColorBrewer
#'
#' @export
honeycomb_generate_maps <- function(
    physician_sf,
    acog_districts_sf,
    trait_map = "all",
    honey_map = "all",
    grid_size = c(0.3, 0.3),
    specific_district = NULL
) {
  options(tigris_use_cache = TRUE)
  sf::sf_use_s2(FALSE)

  # Load USA shapefile
  cat("Loading USA shapefile...\n")
  usa <- rnaturalearth::ne_countries(country = "United States of America", returnclass = "sf")
  usa <- sf::st_transform(usa, 4326)

  # Determine which districts to process
  districts_to_process <- if (!is.null(specific_district)) {
    specific_district
  } else {
    unique(acog_districts_sf$ACOG_District)
  }

  # Loop through each ACOG district to generate hex maps individually
  cat("Loop through each ACOG district to generate hex maps individually...\n")
  lapply(districts_to_process, function(district) {
    cat("Processing district:", district, "...\n")
    district_sf <- dplyr::filter(acog_districts_sf, ACOG_District == district)

    # Create honeycomb grid and intersect with physician data
    honeycomb_grid_sf <- sf::st_make_grid(district_sf, grid_size, what = "polygons", square = FALSE) %>%
      sf::st_sf() %>%
      dplyr::mutate(grid_id = row_number()) %>%
      sf::st_intersection(sf::st_transform(district_sf, 4326))

    intersections <- sf::st_intersection(honeycomb_grid_sf, sf::st_transform(physician_sf, 4326))
    physician_count_per_honey <- intersections %>%
      dplyr::group_by(grid_id) %>%
      dplyr::summarize(physician_count = n(), .groups = 'drop') %>%
      dplyr::filter(physician_count > 1)

    # Join honeycomb grid with physician count
    honeycomb_grid_with_physicians <- sf::st_join(honeycomb_grid_sf, physician_count_per_honey) %>%
      dplyr::filter(!is.na(physician_count))

    # Create the map with honeycomb and ACOG Districts
    cat("Creating the map for district", district, "...\n")

    map_ggplot <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = district_sf, fill = "#D3D3D3", color = "darkblue", size = 1.5) +
      ggplot2::geom_sf(data = honeycomb_grid_with_physicians, aes(fill = physician_count), color = NA) +
      ggplot2::scale_fill_viridis_c(breaks = c(10, 20, 40, 60, 80, 100, 150),
                                    name = "Obstetrics and Gynecology Faculty Subspecialists",
                                    guide = guide_colorbar(direction = "horizontal",
                                                           title.position = "top",
                                                           title.hjust = 0.5,
                                                           label.hjust = 0.5,
                                                           barwidth = 10,
                                                           barheight = 1,
                                                           alpha = 0.7,
                                                           label.theme = element_text(size = 8, color = "black", hjust = 0.5))) +
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

      # Add other theme modifications here if needed
      ggplot2::labs(title = paste("American College of Obstetricians\n and Gynecologists", district)) +
      ggspatial::annotation_scale(location = "bl", width_hint = 0.5, bar_cols = c("black", "white")) +
      ggspatial::annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering()) +
      ggplot2::theme(panel.grid.major = element_line(color = "lightgray")) +
      ggplot2::theme(panel.border = element_rect(color = "darkblue", fill = NA, size = 1.5))

    # Print the plot
    print(map_ggplot)

    # Save the map in various formats
    cat("Saving the map for district", district, "...\n")
    ggplot2::ggsave(filename = paste0("figures/hexmap/hexmap_figures/", trait_map, "_", "_district_", district, "_honey.tiff"), plot = map_ggplot, width = 10, height = 6, dpi = 800)

    return(map_ggplot)
  })
}


#' #' Generate ACOG Districts sf Object
#' #'
#' #' This function generates an sf object representing the ACOG districts by grouping all the states in a particular district together.
#' #'
#' #' @param filepath The path to the CSV file containing ACOG Districts data.
#' #' @return An sf object containing grouped ACOG districts.
#' #' @importFrom sf st_transform st_sf
#' #' @importFrom dplyr left_join group_by summarize
#' #' @importFrom rnaturalearth ne_states
#' #' @importFrom sf sf_use_s2
#' #' @import RColorBrewer
#' #' @import ggplot2
#' #' @export
#'
#' generate_acog_districts_sf <- function() {
#'   options(tigris_use_cache = TRUE)
#'   sf::sf_use_s2(FALSE)
#'
#'   cat("Using ACOG_Districts dataframe...\n")
#'   # Use the ACOG_Districts dataframe from the tyler package
#'   ACOG_Districts <- tyler::ACOG_Districts
#'
#'   cat("Getting the states shapefile...\n")
#'   # Get the states shapefile
#'   states <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
#'   states <- sf::st_transform(states, 4326)
#'
#'   cat("Joining ACOG Districts with states and transforming...\n")
#'   # Join ACOG Districts with states and transform
#'   states <- states %>%
#'     dplyr::left_join(ACOG_Districts, by = c("name" = "State")) %>%
#'     dplyr::mutate(ACOG_District = dplyr::if_else(name == "Alaska" & ACOG_District == "VIII", NA_character_, ACOG_District)) %>%
#'     sf::st_transform(4326)
#'
#'   cat("Grouping states by ACOG District and combining...\n")
#'   # Group states by ACOG District and combine them
#'   ACOG_Districts_sf <- states %>%
#'     dplyr::group_by(ACOG_District) %>%
#'     dplyr::summarize(geometry = sf::st_union(geometry), .groups = 'drop') %>%
#'     sf::st_sf()
#'
#'   cat("ACOG Districts sf creation complete.  Creates a plottable sf file of ACOG districts.\n")
#'   cat("Reference: https://www.acog.org/community/districts-and-sections.\n")
#'
#'   return(ACOG_Districts_sf)
#' }
#'
#' #####
#' #' Generate Hexagon Maps by ACOG District
#' #'
#' #' This function generates hexagon maps for ACOG districts.
#' #'
#' #' @param physician_sf A sf object containing physician data with coordinates.
#' #' @param acog_districts_sf A sf object containing the grouped ACOG districts.
#' #' @param trait_map A string specifying the trait map (default is "all").
#' #' @param honey_map A string specifying the honey map (default is "all").
#' #' @param grid_size A numeric vector of length 2 specifying the grid size for the hexagon map (default is c(0.3, 0.3)).
#' #' @param specific_district A string or NULL to specify a specific district for generating the map (default is NULL, which processes all districts).
#' #' @return A ggplot object of the generated map for the specified or all districts.
#' #' @importFrom sf st_transform st_sf sf_use_s2
#' #' @importFrom dplyr filter group_by summarize
#' #' @importFrom rnaturalearth ne_countries
#' #' @importFrom ggplot2 ggplot geom_sf scale_fill_viridis_c theme_light theme labs ggsave
#' #' @import RColorBrewer
#' #'
#' #'
#' #' @param physician_file A character string specifying the file path to the physician data in RDS format.
#' #' @param acog_districts_file A character string specifying the file path to the ACOG districts data in CSV format.
#' #' @param trait_map A string specifying the trait map (default is "all").
#' #' @param honey_map A string specifying the honey map (default is "all").
#' #' @param grid_size A numeric vector of length 2 specifying the grid size for the hexagon map (default is c(0.3, 0.3)).
#' #' @param specific_district A string or NULL to specify a specific district for generating the map (default is NULL, which processes all districts).
#' #'
#' #' @return A ggplot object of the generated map for the specified or all districts.
#' #'
#' #' @importFrom sf st_transform st_sf sf_use_s2
#' #' @importFrom dplyr filter group_by summarize
#' #' @importFrom rnaturalearth ne_countries
#' #' @importFrom ggplot2 ggplot geom_sf scale_fill_viridis_c theme_light theme labs ggsave
#' #' @import RColorBrewer
#' #'
#' #' @examples
#' #' \dontrun{
#' #' # Generate hexagon maps for a specific district (e.g., District V) using provided data paths
#' #' all_map <- honeycomb_generate_maps(
#' #'   physician_file = "data/Physicians.rds",
#' #'   acog_districts_file = "data/ACOG_Districts.csv",
#' #'   trait_map = "all",
#' #'   honey_map = "all",
#' #'   grid_size = c(0.2, 0.2),
#' #'   specific_district = "District V"
#' #' )
#' #'
#' #' # Save the generated map
#' #' ggsave("all_map.png", all_map, width = 10, height = 6, dpi = 800)
#' #' }
#' #'
#' #' @export
#' #'
#'
#' # Function to generate hexagon maps by ACOG district.
#' honeycomb_generate_maps <- function(physician_file, acog_districts_file, trait_map = "all", honey_map = "all", grid_size = c(0.3, 0.3), specific_district = NULL) {
#'
#'   options(tigris_use_cache = TRUE)
#'   sf::sf_use_s2(FALSE)
#'
#'   # Load physician data
#'   cat("Loading physician data from an RDS file...  Include columns named long and lat...\n")
#'   physicians <- readRDS(physician_file)
#'   physicians <- physicians[!is.na(physicians$long), ]
#'   physician_sf <- sf::st_as_sf(physicians, coords = c("long", "lat"), crs = 4326)
#'
#'   # Load ACOG districts data
#'   cat("Loading ACOG districts data...\n")
#'   #acog_districts_sf <- generate_acog_districts_sf(acog_districts_file)
#'   acog_districts_sf <- generate_acog_districts_sf()
#'
#'   cat("Starting generate_maps function...\n")
#'
#'   # Create the directory if not exists
#'   cat("Checking directory...\n")
#'   if (!dir.exists("figures/hexmap/hexmap_figures")) {
#'     dir.create("figures/hexmap/hexmap_figures", recursive = TRUE)
#'   }
#'
#'   # Load USA shapefile
#'   cat("Loading USA shapefile...\n")
#'   usa <- rnaturalearth::ne_countries(country = "United States of America", returnclass = "sf")
#'   usa <- sf::st_transform(usa, 4326)
#'
#'   # Determine which districts to process
#'   districts_to_process <- if (!is.null(specific_district)) {
#'     specific_district
#'   } else {
#'     unique(acog_districts_sf$ACOG_District)
#'   }
#'
#'   # Loop through each ACOG district to generate hex maps individually
#'   cat("Loop through each ACOG district to generate hex maps individually...\n")
#'   lapply(districts_to_process, function(district) {
#'     cat("Processing district:", district, "...\n")
#'     district_sf <- dplyr::filter(acog_districts_sf, ACOG_District == district)
#'
#'     #Not really working well for me 9/17/2023.  Can't see any lakes.
#'     # Get global rivers and lakes data
#'     #rivers <- rnaturalearth::ne_download(scale = 10, type = 'rivers_lake_centerlines', category = 'physical', returnclass = "sf")
#'     #lakes <- rnaturalearth::ne_download(scale = 10, type = 'lakes', category = 'physical', returnclass = "sf") %>%
#'       #dplyr::filter(name %in% c("Lake Superior", "Lake Michigan", "Lake Huron", "Lake Erie", "Lake Ontario"))
#'     # Clip the rivers and lakes data to the district boundaries
#'     #rivers_in_district <- sf::st_intersection(rivers, district_sf)
#'     #lakes_in_district <- sf::st_intersection(lakes, district_sf)
#'
#'     # Create honeycomb grid and intersect with physician data
#'     honeycomb_grid_sf <- sf::st_make_grid(district_sf, grid_size, what = "polygons", square = FALSE) %>%
#'       sf::st_sf() %>%
#'       dplyr::mutate(grid_id = row_number()) %>%
#'       sf::st_intersection(sf::st_transform(district_sf, 4326))
#'
#'     intersections <- sf::st_intersection(honeycomb_grid_sf, sf::st_transform(physician_sf, 4326))
#'     physician_count_per_honey <- intersections %>%
#'       dplyr::group_by(grid_id) %>%
#'       dplyr::summarize(physician_count = n(), .groups = 'drop') %>%
#'       dplyr::filter(physician_count > 1)
#'
#'     # Join honeycomb grid with physician count
#'     honeycomb_grid_with_physicians <- sf::st_join(honeycomb_grid_sf, physician_count_per_honey) %>%
#'       dplyr::filter(!is.na(physician_count))
#'
#'     # Create the map with honeycomb and ACOG Districts
#'     cat("Creating the map for district", district, "...\n")
#'
#'       map_ggplot <- ggplot2::ggplot() +
#'       ggplot2::geom_sf(data = district_sf, fill = "#D3D3D3", color = "darkblue", size = 1.5) +
#'       ggplot2::geom_sf(data = honeycomb_grid_with_physicians, aes(fill = physician_count), color = NA) +
#'       # # Adding clipped rivers and lakes to your plot
#'       # #geom_sf(data = rivers_in_district, color = "blue", size = 0.5) +
#'       # ggplot2::geom_sf(data = lakes_in_district, fill = "blue", color = "blue") +
#'       ggplot2::scale_fill_viridis_c(breaks = c(10, 20, 40, 60, 80, 100, 150),
#'                            name = "Obstetrics and Gynecology Faculty Subspecialists",
#'                            guide = guide_colorbar(direction = "horizontal",
#'                                                   title.position = "top",
#'                                                   title.hjust = 0.5,
#'                                                   label.hjust = 0.5,
#'                                                   barwidth = 10,
#'                                                   barheight = 1,
#'                                                   alpha = 0.7,
#'                                                   label.theme = element_text(size = 8, color = "black", hjust = 0.5))) +
#'       ggplot2::theme_minimal(base_size = 10) +
#'       ggplot2::theme(
#'         panel.background = element_rect(
#'           fill = "floralwhite",
#'           color = "darkblue",
#'           size = 1,
#'           linetype = "solid"
#'         ),
#'         axis.text = element_text(size = 4, color = "darkgray", family = "Helvetica"),
#'         axis.title = element_text(size = 12, face = "bold", family = "Helvetica"),
#'         plot.title = element_text(size = 14, face = "bold", hjust = 0.5, family = "Helvetica"),
#'         legend.position = "bottom",
#'         legend.title = element_text(face = "bold", color = "darkblue", size = 10, family = "Helvetica"),
#'         legend.text = element_text(size = 8, family = "Helvetica"),
#'         legend.background = element_rect(
#'           fill = "aliceblue",
#'           color = "black",
#'           size = 0.5
#'         ),
#'         plot.margin = margin(10, 10, 10, 10),
#'         legend.key = element_rect(fill = "aliceblue", color = "black", size = 0.5)
#'       ) +
#'
#'       # Add other theme modifications here if needed
#'       ggplot2::labs(title = paste("American College of Obstetricians\n and Gynecologists", district)) +
#'       ggspatial::annotation_scale(location = "bl", width_hint = 0.5, bar_cols = c("black", "white")) +
#'       ggspatial::annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering()) +
#'       ggplot2::theme(panel.grid.major = element_line(color = "lightgray")) +
#'       ggplot2::theme(panel.border = element_rect(color = "darkblue", fill = NA, size = 1.5))
#'
#'     # Print the plot
#'     print(map_ggplot)
#'
#'     # Save the map in various formats
#'     cat("Saving the map for district", district, "...\n")
#'     ggplot2::ggsave(filename = paste0("figures/hexmap/hexmap_figures/", trait_map, "_", "_district_", district, "_honey.tiff"), plot = map_ggplot, width = 10, height = 6, dpi = 800)
#'
#'     return(map_ggplot)
#'   })
#' }
#'
#' # Use case:
#' #
#' # generate_acog_districts_sf("inst/extdata/ACOG_Districts.csv")
#' # generate_acog_districts_sf()
#' #
#' # all_map <-
#' #   tyler::honeycomb_generate_maps(
#' #     physician_file = "inst/extdata/Physicians.rds",
#' #     acog_districts_file = "inst/extdata/ACOG_Districts.csv",
#' #     trait_map = "all",
#' #     honey_map = "all",
#' #     grid_size = c(0.2, 0.2),
#' #     specific_district = "District V"
#' #   )
