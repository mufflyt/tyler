## ----setup, include=FALSE-----------------------------------------------------
eval_vignettes <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
knitr::opts_chunk$set(
  echo = TRUE,
  comment = "#>",
  collapse = TRUE,
  eval = eval_vignettes
)

if (eval_vignettes) {
  library(mysterycall)
}

## ----eval=FALSE---------------------------------------------------------------
# Sys.setenv(HERE_API_KEY = "your-here-key")

## ----eval=FALSE---------------------------------------------------------------
# library(mysterycall)
# 
# # 1. Geocode practice addresses → lat/long
# geocoded <- mysterycall_geocode(
#   file_path           = "data/providers_with_addresses.csv",
#   google_maps_api_key = Sys.getenv("GOOGLE_MAPS_API_KEY"),
#   output_file_path    = "data/geocoded_providers.csv"
# )
# 
# # 2. Rename coordinate columns to lat / long
# iso_input <- geocoded |>
#   dplyr::rename(lat = latitude, long = longitude)
# 
# # 3. Write to a file (easyr::read.any() will read it back)
# readr::write_csv(iso_input, "data/iso_input.csv")
# 
# # 4. Generate isochrones
# isochrones_sf <- create_isochrones_for_dataframe(
#   input_file    = "data/iso_input.csv",
#   breaks        = c(30 * 60, 60 * 60, 120 * 60, 180 * 60),  # 30/60/120/180 min
#   api_key       = Sys.getenv("HERE_API_KEY"),
#   output_dir    = "data/isochrone_checkpoints",
#   save_interval = 100   # save checkpoint every 100 providers
# )

## ----eval=FALSE---------------------------------------------------------------
# # Inspect the result
# names(isochrones_sf)
# 
# dplyr::count(isochrones_sf, travel_time_minutes)
# #>   travel_time_minutes   n
# #> 1                  30 498
# #> 2                  60 498
# #> 3                 120 497
# #> 4                 180 497
# # (some providers may return fewer polygons if the API cannot reach them)
# 
# # Save the full sf object
# sf::st_write(isochrones_sf, "data/isochrones.gpkg", delete_dsn = TRUE)
# readr::write_rds(isochrones_sf, "data/isochrones.rds")

## ----eval=FALSE---------------------------------------------------------------
# # Load the most recent checkpoint
# checkpoints <- list.files("data/isochrone_checkpoints", pattern = "\\.rds$",
#                            full.names = TRUE)
# partial <- readr::read_rds(sort(checkpoints, decreasing = TRUE)[1])
# 
# completed_idx <- unique(partial$point_index)
# message("Resuming from row ", max(completed_idx) + 1,
#         " (", length(completed_idx), " rows already done)")
# 
# # Pass the source file again; the function will skip completed indices
# isochrones_sf <- create_isochrones_for_dataframe(
#   input_file    = "data/iso_input.csv",
#   breaks        = c(30 * 60, 60 * 60, 120 * 60, 180 * 60),
#   api_key       = Sys.getenv("HERE_API_KEY"),
#   output_dir    = "data/isochrone_checkpoints",
#   save_interval = 100,
#   skip_indices  = completed_idx   # omit already-processed rows
# )
# 
# # Combine the checkpoint and the new results
# isochrones_sf <- dplyr::bind_rows(partial, isochrones_sf)

## ----eval=FALSE---------------------------------------------------------------
# # Load the enriched block group layer (built in the Census vignette)
# bg_sf <- readr::read_rds("data/block_groups_with_demographics.rds")
# 
# # Calculate intersection overlap and save results
# overlap_results <- calculate_intersection_overlap_and_save(
#   isochrones_sf = isochrones_sf,
#   bg_sf         = bg_sf,
#   output_dir    = "data/overlap_results"
# )

