# Load the function
source(file = "R/honeycomb_map_by_acog_districts.R", echo = FALSE)

# Instructions for using the function
cat("\033[How to create ACOG District Maps that can have hexagons too:\033[0m\n")
cat("\033[34mInstructions:\n",
    "all_map <- generate_maps(physician_sf,\n",
    "                   acog_districts_sf,\n",
    "                   trait_map = 'all',\n",
    "                   honey_map = 'all',\n",
    "                   grid_size = c(0.2, 0.2),\n",
    "                   specific_district = 'District V')\n", "\033[0m")

# all_map <-
#   tyler::generate_maps(
#     physician_file = "inst/extdata/Physicians.rds",
#     acog_districts_file = "inst/extdata/ACOG_Districts.csv",
#     trait_map = "all",
#     honey_map = "all",
#     grid_size = c(0.2, 0.2),
#     specific_district = "District V"
#   )
