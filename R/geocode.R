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
#' @param failed_output_path Optional path that captures rows that failed to
#'   geocode after all retries. When supplied, a timestamped backup is created
#'   before overwriting existing results.
#' @param notify Logical. If `TRUE`, play a notification sound when geocoding
#'   finishes (requires the optional `beepr` package). Defaults to `TRUE`.
#' @param quiet Logical flag controlling log verbosity. Defaults to the package
#'   quiet-mode option.
#' @param tracker Optional progress tracker created with [progress_tracker()].
#'   When supplied, the step named by `tracker_step` is automatically started and
#'   marked as complete or failed with an appropriate quality tier.
#' @param tracker_step Character string describing the step name used when
#'   updating `tracker`.
#'
#' @return A data frame with latitude and longitude columns added.
#' @export
#' @examples
#' \dontrun{
#' result <- geocode_unique_addresses("addresses.csv", "my_api_key")
#' }
#' @importFrom readr read_csv write_csv
#' @importFrom readxl read_excel
#' @importFrom dplyr left_join distinct mutate
#' @importFrom tibble tibble
#' @importFrom stats complete.cases
#'
geocode_unique_addresses <- function(file_path, google_maps_api_key,
                                     output_file_path = NULL,
                                     failed_output_path = NULL,
                                     notify = TRUE,
                                     quiet = getOption("tyler.quiet", FALSE),
                                     tracker = NULL,
                                     tracker_step = "Geocoding") {
  checkmate::assert_string(file_path, min.chars = 1, any.missing = FALSE)
  checkmate::assert_file_exists(file_path)
  checkmate::assert_string(google_maps_api_key, min.chars = 1, any.missing = FALSE)
  checkmate::assert_flag(notify)
  checkmate::assert_flag(quiet)
  checkmate::assert_string(tracker_step, min.chars = 1, any.missing = FALSE)
  if (!is.null(output_file_path)) {
    checkmate::assert_string(output_file_path, min.chars = 1, any.missing = FALSE)
    checkmate::assert_true(dirname(output_file_path) != "", .var.name = "output_file_path directory")
  }
  if (!is.null(failed_output_path)) {
    checkmate::assert_string(failed_output_path, min.chars = 1, any.missing = FALSE)
    checkmate::assert_true(dirname(failed_output_path) != "", .var.name = "failed_output_path directory")
  }

  # Read input based on extension
  ext <- tools::file_ext(file_path)
  data <- switch(tolower(ext),
                 csv = readr::read_csv(file_path, show_col_types = FALSE),
                 rds = readRDS(file_path),
                 xlsx = readxl::read_excel(file_path),
                 stop("Unsupported file type: ", ext))

  checkmate::assert_data_frame(data, min.rows = 1)
  checkmate::assert_names(names(data), must.include = "address")
  checkmate::assert_true(all(!is.na(data$address)), .var.name = "address has no missing values")
  checkmate::assert_true(all(trimws(data$address) != ""), .var.name = "address has no blank values")

  if (!requireNamespace("ggmap", quietly = TRUE)) {
    stop("Package 'ggmap' is required for geocode_unique_addresses(). Install with: install.packages('ggmap')", call. = FALSE)
  }
  ggmap::register_google(key = google_maps_api_key)

  unique_add <- dplyr::distinct(data, address)
  total_unique <- nrow(unique_add)
  checkmate::assert_int(total_unique, lower = 0)
  checkmate::assert_true(total_unique <= nrow(data), .var.name = "unique address count")
  tyler_log_info(sprintf("Geocoding %d unique address(es).", total_unique), quiet = quiet)

  # Comprehensive logging: show what we're about to do
  if (!quiet) {
    tyler_log_info(sprintf("Found %d total address records, %d unique", nrow(data), total_unique), indent = TRUE)
  }

  if (!is.null(tracker) && inherits(tracker, "tyler_progress_tracker")) {
    progress_tracker_start(tracker, tracker_step, note = sprintf("%d unique address(es)", total_unique))
  }

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
  coords <- tibble::tibble(lat = numeric(), lon = numeric())
  if (total_unique) {
    first_address <- unique_add$address[[1]]
    for (attempt in seq_len(max_attempts)) {
      attempt_result <- tryCatch({
        ggmap::geocode(unique_add$address, key = google_maps_api_key)
      }, error = function(e) e)

      if (inherits(attempt_result, "error")) {
        reason <- extract_status(attempt_result$message)
        tyler_log_info(sprintf(
          "Attempt %d/%d for address '%s' failed %s.",
          attempt,
          max_attempts,
          first_address,
          reason
        ), quiet = quiet)

        if (attempt == max_attempts) {
          failure_reason <- sprintf("Geocoding failed after %d attempts: %s", max_attempts, attempt_result$message)
          if (!is.null(failed_output_path)) {
            failure_tbl <- tibble::tibble(address = unique_add$address, reason = failure_reason)
            tyler_export_with_backup(failure_tbl, failed_output_path, quiet = quiet)
          }
          if (!is.null(tracker) && inherits(tracker, "tyler_progress_tracker")) {
            progress_tracker_fail(tracker, tracker_step, reason = failure_reason)
          }
          stop(failure_reason)
        }

        delay <- base_delay * 2^(attempt - 1)
        tyler_log_info(sprintf("Retrying geocode request in %.1f seconds...", delay), quiet = quiet)
        Sys.sleep(delay)
      } else {
        coords <- attempt_result

        # Validate geocoding result structure immediately (Bug #11 fix)
        if (!is.data.frame(coords)) {
          stop("Geocoding API returned unexpected data type (expected data frame).")
        }
        if (!"lat" %in% names(coords) || !"lon" %in% names(coords)) {
          stop(sprintf(
            "Geocoding API returned unexpected structure. Expected 'lat' and 'lon' columns, got: %s",
            paste(names(coords), collapse = ", ")
          ))
        }
        checkmate::assert_true(length(coords$lat) == nrow(coords), .var.name = "lat length")
        checkmate::assert_true(length(coords$lon) == nrow(coords), .var.name = "lon length")
        checkmate::assert_numeric(coords$lat, any.missing = TRUE, finite = FALSE, .var.name = "coords$lat")
        checkmate::assert_numeric(coords$lon, any.missing = TRUE, finite = FALSE, .var.name = "coords$lon")
        invalid_coord_rows <- which((!is.na(coords$lat) & (coords$lat < -90 | coords$lat > 90)) |
                                      (!is.na(coords$lon) & (coords$lon < -180 | coords$lon > 180)))
        if (length(invalid_coord_rows)) {
          warning(sprintf(
            "Geocoding returned %d row(s) with out-of-bounds coordinates; setting those results to NA.",
            length(invalid_coord_rows)
          ))
          coords$lat[invalid_coord_rows] <- NA_real_
          coords$lon[invalid_coord_rows] <- NA_real_
        }
        if (nrow(coords) != total_unique) {
          warning(sprintf(
            "Geocoding returned %d rows but expected %d. Some addresses may be missing results.",
            nrow(coords),
            total_unique
          ))
        }

        break
      }
    }
  }

  unique_add <- dplyr::mutate(unique_add,
                              latitude = coords$lat,
                              longitude = coords$lon)

  failed_rows <- unique_add[!stats::complete.cases(unique_add[, c("latitude", "longitude")]), , drop = FALSE]
  success_rate <- if (total_unique) 1 - nrow(failed_rows) / total_unique else 1
  success_count <- total_unique - nrow(failed_rows)
  checkmate::assert_number(success_rate, lower = 0, upper = 1, .var.name = "success_rate")
  checkmate::assert_int(success_count, lower = 0, upper = total_unique, .var.name = "success_count")

  # Comprehensive logging: Report geocoding results
  if (!quiet) {
    if (success_rate >= 0.95) {
      tyler_log_success(sprintf("Geocoding complete: %d/%d succeeded (%.1f%%)",
                               success_count, total_unique, success_rate * 100),
                       indent = TRUE)
    } else if (success_rate >= 0.80) {
      tyler_log_warning(sprintf("Geocoding finished with warnings: %d/%d succeeded (%.1f%%)",
                               success_count, total_unique, success_rate * 100),
                       indent = TRUE)
    } else {
      tyler_log_error(sprintf("Geocoding had low success rate: %d/%d succeeded (%.1f%%)",
                             success_count, total_unique, success_rate * 100),
                     indent = TRUE)
    }
  }

  if (nrow(failed_rows) && !is.null(failed_output_path)) {
    tyler_export_with_backup(failed_rows, failed_output_path, quiet = quiet)
    tyler_log_info(sprintf("Exported %d failed address(es) to %s", nrow(failed_rows), failed_output_path), quiet = quiet)
  }

  if (!is.null(tracker) && inherits(tracker, "tyler_progress_tracker")) {
    progress_tracker_finish(tracker, tracker_step, score = success_rate, note = sprintf("%d/%d succeeded", success_count, total_unique))
  }

  data <- dplyr::left_join(data, unique_add, by = "address")

  if (!is.null(output_file_path)) {
    tyler_export_with_backup(data, output_file_path, quiet = quiet, backup = TRUE)
    if (!quiet) {
      tyler_log_save(output_file_path, n_rows = nrow(data))
    }
  }

  if (isTRUE(notify) && requireNamespace("beepr", quietly = TRUE)) {
    beepr::beep(2)
  }
  data
}
