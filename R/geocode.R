#' Geocode Unique Addresses from a CSV File
#'
#' This function reads a CSV file containing addresses, geocodes unique addresses using Google Maps API,
#' and writes the geocoded data back to a CSV file.
#'
#' @param file_path The path to the CSV file containing the addresses. The file should have a column named "address".
#' @param google_maps_api_key Your Google Maps API key for geocoding.
#' @param output_file_path The path where the output CSV file with geocoded data will be saved.
#'
#' @return A data frame containing the original data along with latitude and longitude for each unique address.
#' @export
#'
#' @examples
#' \dontrun{
#' google_maps_api_key <- "your_google_maps_api_key"
#' output_data <- geocode_unique_addresses("path/to/your/input/file.csv", google_maps_api_key, "path/to/your/output/file.csv")
#' }
#'


geocode_unique_addresses <- function(file_path, google_maps_api_key, output_file_path) {

  # Load the required packages
  library(ggmap)
  library(tidyverse)

  # Register the Google Maps API key
  # Google geocoding ----
  #https://www.jessesadler.com/post/geocoding-with-r/
  #Google map API, https://console.cloud.google.com/google/maps-apis/overview?pli=1
  register_google(key = google_maps_api_key)

  # Read the data
  data <- read_csv(file_path)

  # Filter unique addresses
  unique_addresses <- distinct(data, address)

  # Geocode the unique addresses
  geocoded_data <- unique_addresses %>%
    rowwise() %>%
    mutate(geocode_result = list(geocode(address, output = "latlon", source = "google"))) %>%
    unnest_wider(geocode_result) %>%
    select(-address, address, lon, lat)

  # Join the geocoded data back with the original data
  final_data <- left_join(data, geocoded_data, by = "address")

  # Write the geocoded data back to a CSV
  write_csv(final_data, output_file_path)

  # Return the final data
  return(final_data)
}


# Example usage
# Assuming the CSV has a column called "address"


#output_data <- geocode_unique_addresses(file_path = "/Users/tylermuffly/Dropbox (Personal)/Tannous/data/address_for_geocoding.csv", google_maps_api_key = "AIzaSyCqYltnRJS2B8KiXXgTMkmDb5mUiMgxScU", output_file_path = "/Users/tylermuffly/Dropbox (Personal)/Tannous/data/geocoded_unique_addresses.csv")

