#' Create data
#'
#' @param data the land area dataset
#' @param filename land_area
#' @param title land_area
#' @param description The description of the dataset
#' @param vars A named list or vector of the column descriptions
#' @param data_URL The URL for the data source
#' @param dataname The name of the data (taken from the data argument if not set)
#'
#' @return Saves to file or text output
#'
#' @export
#'
#' @examples
#' land_area <- land_area
#'
#'   \item{state_name}{State}
#'   \item{total_area_sq_mi}{Total area square miles}
#'   \item{total_area_sq_km}{Total area square kilometers}
#'   \item{land_area_sq_mi}{Land area square miles}
#'   \item{sq_km_2}{}
#'   \item{sq_mi_3}{}
#'   \item{total_sq_km}{}
#'   \item{inland_sq_mi}{inland square miles}
#'   \item{inland_sq_km}{}
#'   \item{coastal_sq_mi}{}
#'   \item{coastal_sq_km}{}
#'   \item{great_lakes_sq_mi}{great lakes square miles}
#'   \item{great_lakes_sq_km}{great lakes square kilometers}
#'   \item{territorial_sq_mi}{territorial square miles}
#'   \item{territorial_sq_km}{territorial square kilometers}
#'   \item{internal_point_latitude_value}{latitude}
#'   \item{internal_point_longitude_value}{longitude}

library(readr)

column_types <- cols(
  State_and_other_areas2 = col_character(),
  `total area, Sq_Mi` = col_number(),
  `total area, Sq_Km` = col_number(),
  `land area, Sq_Mi` = col_number(),
  `land area, Sq_Km` = col_number(),
  `water area, Sq_Mi` = col_number(),
  `water area, Sq_Km` = col_number(),
  `Inland,Sq_Mi` = col_number(),
  `Inland, Sq_Km` = col_number(),
  `Coastal, Sq_Mi` = col_number(),
  `Coastal, Sq_Km` = col_number(),
  `Great lakes, Sq_Mi` = col_number(),
  `Great lakes, Sq_Km` = col_number(),
  `Territorial, Sq_Mi` = col_number(),
  `Territorial, Sq_Km` = col_number(),
  `Internal_Point_Latitude_Value` = col_double(),
  `Internal_Point_Longitude_Value` = col_double()
)

# Read the RDS file with specified column types
land_area <- read_csv("~/Dropbox (Personal)/workforce/Master_References/Land_area/Land_area.csv",
                      col_types = column_types)

# Clean and rename column names
land_area <- land_area %>%
  janitor::clean_names(case = "snake", allow_dupes = FALSE) %>%
  dplyr::rename(state_name = state_and_other_areas2)

#usethis::use_data(land_area, overwrite = TRUE)
