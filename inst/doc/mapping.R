knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

# library(tyler)
# library(leaflet)
# 
# # Default view: continental United States, zoom 4
# map_create_base()
# 
# # With a custom HTML title
# map_create_base(title = "<strong>Ob-Gyn Provider Map</strong>")
# 
# # Zoom in on a specific region
# map_create_base(
#   title = "Mountain West",
#   lat   = 39.5,
#   lng   = -111.0,
#   zoom  = 6
# )

# library(tyler)
# 
# # Build the sf object (requires rnaturalearth)
# districts_sf <- map_create_acog_districts_sf()
# 
# # Inspect the result
# class(districts_sf)
# #> [1] "sf"         "tbl_df"     "tbl"        "data.frame"
# 
# names(districts_sf)
# #> [1] "ACOG_District"      "Subregion"          "States"
# #> [4] "State_Abbreviations" "geometry"
# 
# print(districts_sf[, c("ACOG_District", "States")], n = 5)

library(tyler)

data(ACOG_Districts)
head(ACOG_Districts)

# library(tyler)
# 
# # Synthetic provider data — replace with your own data frame
# physician_data <- data.frame(
#   long          = c(-95.363, -97.743, -98.494, -96.900, -95.370,
#                     -87.623, -122.431, -73.935, -84.388, -104.990),
#   lat           = c( 29.763,  30.267,  29.424,  32.779,  29.752,
#                      41.883,  37.774,  40.730,  33.749,  39.739),
#   name          = paste("Physician", 1:10),
#   ACOG_District = c(
#     "District VII", "District VII", "District VII", "District VII", "District VII",
#     "District IV",  "District IX",  "District II",  "District IV",  "District VIII"
#   )
# )
# 
# # Create the dot map and save HTML + PNG to a temporary directory
# map_create_physician_dot(
#   physician_data = physician_data,
#   jitter_range   = 0.05,
#   color_palette  = "magma",
#   popup_var      = "name",
#   output_dir     = tempdir()
# )

# library(tyler)
# library(ggplot2)
# 
# hrr_sf <- hrr(remove_HI_AK = TRUE)
# 
# # Quick look at the object
# glimpse(hrr_sf)

# ggplot(hrr_sf) +
#   geom_sf(fill = "#f0f0f0", color = "#999999", linewidth = 0.15) +
#   theme_void() +
#   labs(title = "Hospital Referral Regions — Continental United States")

# # physician_sf must be an sf object with point geometries
# hrr_generate_maps(
#   physician_sf = physician_sf,
#   trait_map    = "obgyn",
#   honey_map    = "all",
#   output_dir   = tempdir(),
#   dpi          = 300,
#   width        = 7,
#   height       = 5
# )

# library(tyler)
# 
# # bg_data and isochrones_data are sf objects produced by the isochrone workflow
# map_create_block_group_overlap(
#   bg_data        = bg_data,
#   isochrones_data = isochrones_data,
#   output_dir     = tempdir()
# )
