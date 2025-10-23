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
#' @param notify Logical. If `TRUE`, play a notification sound when geocoding
#'   finishes (requires the optional `beepr` package). Defaults to `TRUE`.
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
                                     output_file_path = NULL,
                                     notify = TRUE) {
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

  extract_status <- function(msg) {
    code <- regmatches(msg, regexpr("\\b[0-9]{3}\\b", msg))
    if (length(code) && nzchar(code)) {
      return(paste("after", code))
    }
    msg_clean <- gsub("\\s+", " ", msg)
    paste("due to", substr(msg_clean, 1, 120))
  }

  max_attempts <- 3L
  base_delay <- 1
  coords <- NULL
  first_address <- if (nrow(unique_add)) unique_add$address[[1]] else "address list"

  for (attempt in seq_len(max_attempts)) {
    attempt_result <- tryCatch({
      ggmap::geocode(unique_add$address, key = google_maps_api_key)
    }, error = function(e) e)

    if (inherits(attempt_result, "error")) {
      reason <- extract_status(attempt_result$message)
      message(sprintf(
        "Attempt %d/%d for address '%s' failed %s.",
        attempt,
        max_attempts,
        first_address,
        reason
      ))

      if (attempt == max_attempts) {
        stop(sprintf("Geocoding failed after %d attempts: %s", max_attempts, attempt_result$message))
      }

      delay <- base_delay * 2^(attempt - 1)
      message(sprintf("Retrying geocode request in %.1f seconds...", delay))
      Sys.sleep(delay)
    } else {
      coords <- attempt_result
      break
    }
  }

  unique_add <- dplyr::mutate(unique_add,
                              latitude = coords$lat,
                              longitude = coords$lon)

  data <- dplyr::left_join(data, unique_add, by = "address")

  if (!is.null(output_file_path)) {
    readr::write_csv(data, output_file_path)
  }

  if (isTRUE(notify) && requireNamespace("beepr", quietly = TRUE)) {
    beepr::beep(2)
  }
  data
}
