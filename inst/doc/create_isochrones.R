eval_vignettes <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
knitr::opts_chunk$set(
  echo = TRUE,
  comment = "#>",
  collapse = TRUE,
  eval = eval_vignettes
)

if (eval_vignettes) {
  library(tyler)
}

# Sys.setenv(HERE_API_KEY = "your-here-key")

# iso_input <- geocoded |>
#   dplyr::rename(
#     lat = latitude,
#     long = longitude
#   )
# 
# input_csv <- tempfile(fileext = ".csv")
# readr::write_csv(iso_input, input_csv)
# 
# isochrones <- create_isochrones_for_dataframe(
#   input_file = input_csv,
#   breaks = c(30 * 60, 60 * 60, 120 * 60, 180 * 60),
#   api_key = Sys.getenv("HERE_API_KEY"),
#   output_dir = tempfile(),
#   save_interval = 240
# )

# names(isochrones)
# 
# isochrones |>
#   dplyr::count(travel_time_minutes)
