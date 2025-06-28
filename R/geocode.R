#' Geocode unique addresses from a file
#'
#' Reads an input dataset containing an `address` column, geocodes the unique
#' addresses using the Google Maps API via `ggmap::geocode`, and returns the
#' data with additional `latitude` and `longitude` columns. Optionally writes the
#' result to `output_file_path`.
#'
#' @param file_path Path to a CSV, RDS or XLSX file containing an `address`
#'   column.
#' @param google_maps_api_key A valid Google Maps API key.
#' @param output_file_path Optional path to save the geocoded dataset as CSV.
#'
#' @return A data frame with latitude and longitude columns added.
#' @export
#' @examples
#' \dontrun{
#' result <- geocode_unique_addresses("addresses.csv", "my_api_key")
#' }
#' @importFrom ggmap geocode register_google
#' @importFrom readr read_csv write_csv
#' @importFrom readxl read_excel
#' @importFrom dplyr left_join distinct mutate
#'
geocode_unique_addresses <- function(file_path, google_maps_api_key,
                                     output_file_path = NULL) {
  if (!file.exists(file_path)) {
    stop("Input file not found.")
  }

  # Read input based on extension
  ext <- tools::file_ext(file_path)
  data <- switch(tolower(ext),
                 csv = readr::read_csv(file_path, show_col_types = FALSE),
                 rds = readRDS(file_path),
                 xlsx = readxl::read_excel(file_path),
                 stop("Unsupported file type: ", ext))

  if (!"address" %in% names(data)) {
    stop("The dataset must have a column named 'address' for geocoding.")
  }

  ggmap::register_google(key = google_maps_api_key)

  unique_add <- dplyr::distinct(data, address)
  coords <- ggmap::geocode(unique_add$address, key = google_maps_api_key)
  unique_add <- dplyr::mutate(unique_add,
                              latitude = coords$lat,
                              longitude = coords$lon)

  data <- dplyr::left_join(data, unique_add, by = "address")

  if (!is.null(output_file_path)) {
    readr::write_csv(data, output_file_path)
  }

  data
}
