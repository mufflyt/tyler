#' Create `sf` Polygons for ACOG Districts
#'
#' This helper reads the packaged ACOG district lookup table and joins it with
#' state geometries from Natural Earth to construct polygons for each district.
#' The resulting object can be used to add district boundaries to Leaflet maps
#' or for further spatial analysis.
#'
#' @param acog_districts_file Optional path to a CSV containing the mapping of
#'   states to ACOG districts. Defaults to the packaged
#'   `inst/extdata/ACOG_Districts.csv`.
#'
#' @return An `sf` object with one row per ACOG district and columns describing
#'   the district name, subregion, member states, and their abbreviations.
#'
#' @family mapping
#' @export
#' @examples
#' \dontrun{
#' map_create_acog_districts_sf()
#' map_create_acog_districts_sf("inst/extdata/ACOG_Districts.csv")
#' }
map_create_acog_districts_sf <- function(acog_districts_file = NULL) {
  if (is.null(acog_districts_file)) {
    acog_districts_file <- system.file("extdata", "ACOG_Districts.csv", package = "tyler")
  }

  if (!nzchar(acog_districts_file) || !file.exists(acog_districts_file)) {
    stop("Could not locate the ACOG districts file at '", acog_districts_file, "'.")
  }

  districts <- readr::read_csv(acog_districts_file, show_col_types = FALSE, progress = FALSE)

  names(districts) <- gsub("^\\ufeff", "", names(districts))
  if ("ï..State" %in% names(districts)) {
    districts <- dplyr::rename(districts, State = "ï..State")
  }

  if (!"State" %in% names(districts)) {
    stop("The ACOG districts file must contain a 'State' column.")
  }

  districts <- dplyr::mutate(
    districts,
    State = stringr::str_trim(State),
    ACOG_District = stringr::str_trim(ACOG_District),
    Subregion = dplyr::coalesce(stringr::str_trim(Subregion), ACOG_District)
  )

  states_sf <- rnaturalearth::ne_states(country = "united states of america", returnclass = "sf")
  states_sf <- dplyr::transmute(
    states_sf,
    State = stringr::str_trim(name),
    postal,
    geometry
  )

  states_sf <- dplyr::filter(states_sf, State %in% districts$State)

  states_with_districts <- dplyr::left_join(states_sf, districts, by = "State")
  states_with_districts <- dplyr::filter(states_with_districts, !is.na(ACOG_District))

  if (!nrow(states_with_districts)) {
    stop("No matching states were found when joining Natural Earth geometries to the district table.")
  }

  states_with_districts <- dplyr::mutate(
    states_with_districts,
    State_Abbreviations = dplyr::coalesce(State_Abbreviations, postal)
  )

  districts_sf <- states_with_districts %>%
    dplyr::group_by(ACOG_District, Subregion) %>%
    dplyr::summarise(
      States = paste(sort(unique(State)), collapse = ", "),
      State_Abbreviations = paste(sort(unique(State_Abbreviations)), collapse = ", "),
      geometry = sf::st_union(geometry),
      .groups = "drop"
    ) %>%
    dplyr::arrange(ACOG_District)

  sf::st_as_sf(districts_sf)
}

