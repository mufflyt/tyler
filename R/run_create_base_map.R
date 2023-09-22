# Load the function
source("R/create_base_map.R")

# Instructions for using the function
cat("\033[34mInstructions:\033[0m\n")
cat("\033[34mTo create a base map, use the following code:\033[0m\n")
cat("\033[34mmy_map <- create_base_map(\"<h1>Custom Map Title</h1>\")\n")
cat("\033[34mTo display the map and add circle markers, use the following code:\033[0m\n")
cat("\033[34mmy_map <- my_map %>%\n")
cat("  leaflet::addCircleMarkers(lng = ~longitude,\n")
cat("                            lat = ~latitude,\n")
cat("                            data = data_points,\n")
cat("                            popup = ~popup_text,\n")
cat("                            radius = ~radius,\n")
cat("                            color = ~color,\n")
cat("                            fill = TRUE,\n")
cat("                            stroke = FALSE,\n")
cat("                            fillOpacity = 0.8)\n\n\033[0m")

# Example usage
#my_map <- tyler::create_base_map("<h1>Custom Map Title</h1>")

