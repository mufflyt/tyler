## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----base-map, eval=FALSE-----------------------------------------------------
# library(mysterycall)
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

## ----acog-districts, eval=FALSE-----------------------------------------------
# library(mysterycall)
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

## ----acog-data, eval=TRUE-----------------------------------------------------
library(mysterycall)

data(ACOG_Districts)
head(ACOG_Districts)

## ----acog-summary-table, eval=TRUE--------------------------------------------
data(ACOG_Districts)
dist_summary <- aggregate(State ~ ACOG_District, data = ACOG_Districts, FUN = length)
names(dist_summary)[2] <- "n_states"
dist_summary <- dist_summary[order(dist_summary$ACOG_District), ]
knitr::kable(
  dist_summary,
  col.names = c("ACOG District", "States"),
  caption   = "Number of US states (and territories) in each ACOG district.",
  row.names = FALSE
)

## ----acog-district-bar, eval=TRUE, fig.width=6, fig.height=3.8, fig.cap="State count per ACOG district. Districts vary from 2 (District XI — Puerto Rico / US territories) to 8 states."----
data(ACOG_Districts)
dist_counts <- aggregate(State ~ ACOG_District, data = ACOG_Districts, FUN = length)
names(dist_counts)[2] <- "n_states"
dist_counts$ACOG_District <- factor(
  dist_counts$ACOG_District,
  levels = dist_counts$ACOG_District[order(dist_counts$n_states)]
)

ggplot2::ggplot(dist_counts,
    ggplot2::aes(x = n_states, y = ACOG_District, fill = n_states)) +
  ggplot2::geom_col(width = 0.7) +
  ggplot2::geom_text(ggplot2::aes(label = n_states), hjust = -0.2, size = 3.2) +
  ggplot2::scale_fill_gradient(low = "#c6dbef", high = "#2171b5", guide = "none") +
  ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
  ggplot2::labs(x = "Number of states", y = NULL) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())

## ----physician-dot, eval=FALSE------------------------------------------------
# library(mysterycall)
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

## ----hrr-sf, eval=FALSE-------------------------------------------------------
# library(mysterycall)
# library(ggplot2)
# 
# hrr_sf <- hrr(remove_HI_AK = TRUE)
# 
# # Quick look at the object
# glimpse(hrr_sf)

## ----hrr-map, eval=FALSE------------------------------------------------------
# ggplot(hrr_sf) +
#   geom_sf(fill = "#f0f0f0", color = "#999999", linewidth = 0.15) +
#   theme_void() +
#   labs(title = "Hospital Referral Regions — Continental United States")

## ----hrr-hex, eval=FALSE------------------------------------------------------
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

## ----block-group-overlap, eval=FALSE------------------------------------------
# library(mysterycall)
# 
# # bg_data and isochrones_data are sf objects produced by the isochrone workflow
# map_create_block_group_overlap(
#   bg_data        = bg_data,
#   isochrones_data = isochrones_data,
#   output_dir     = tempdir()
# )

