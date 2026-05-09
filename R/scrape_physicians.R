#' Scrape Physicians' Data with Tor
#'
#' Scrapes board-certification data for physicians within a specified ID range
#' from the ABOG API, routing all requests through a local Tor SOCKS proxy to
#' anonymise the source IP. IDs listed in `wrong_ids_path` are skipped.
#'
#' @param startID The starting diplomate ID for scraping.
#' @param endID The ending diplomate ID for scraping.
#' @param torPort The port number for the local Tor SOCKS proxy (typically 9150).
#' @param wrong_ids_path Optional path to a CSV or Parquet file containing an
#'   integer `WrongIDs` column listing IDs to skip.
#' @param timeout_sec Number of seconds before an individual HTTP request times
#'   out. Defaults to 30.
#' @param delay_sec Seconds to pause between successive requests. Defaults to 1.
#' @param output_dir Directory where result files are written. Defaults to
#'   `tempdir()`. Pass `NULL` to skip all file I/O and only return the data.
#' @param output_format File format for saving results: `"csv"` (default) or
#'   `"parquet"`.
#'
#' @return A data frame containing scraped physician records, invisibly. When
#'   `output_dir` is not `NULL`, result files are also written there and their
#'   paths reported via `message()`.
#'
#' @importFrom httr GET content use_proxy timeout
#' @importFrom dplyr bind_rows
#' @importFrom readr read_csv write_csv
#'
#' @family npi
#'
#' @examplesIf interactive()
#' # Requires a running Tor proxy and network access.
#' scrape_physicians_data_with_tor(startID = 9045999, endID = 9046000, torPort = 9150)
#'
scrape_physicians_data_with_tor <- function(startID, endID, torPort,
                                            wrong_ids_path = NULL,
                                            timeout_sec = 30,
                                            delay_sec = 1,
                                            output_dir = tempdir(),
                                            output_format = c("csv", "parquet")) {
  output_format <- match.arg(output_format)
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required for this function. Install with: install.packages('jsonlite')", call. = FALSE)
  }
  message("Starting scrape_physicians_data_with_tor...")
  message("Parameters - startID: ", startID, "  endID: ", endID, "  torPort: ", torPort)

  id_list <- seq(startID, endID)
  message("ID range: ", startID, " to ", endID, " (", length(id_list), " IDs)")

  wrongs1 <- if (!is.null(wrong_ids_path)) mysterycall_read_table(wrong_ids_path) else data.frame(WrongIDs = integer())
  wrong_ids <- wrongs1$WrongIDs
  if (length(wrong_ids)) message("Skipping ", length(wrong_ids), " known bad IDs.")

  Physicians <- data.frame()
  WrongIDs   <- c()

  base     <- "https://api.abog.org/"
  endpoint <- "diplomate/"
  action   <- "/verify"

  for (id in id_list) {
    if (id %in% wrong_ids) next

    if (!is.numeric(id) || is.na(id)) {
      message("Invalid ID: ", id, " — skipping")
      next
    }

    id_clean <- as.character(as.integer(id))
    url      <- paste0(base, endpoint, id_clean, action)

    ph_r <- tryCatch(
      httr::GET(url,
                httr::use_proxy(paste0("socks5://localhost:", torPort)),
                httr::timeout(timeout_sec)),
      error = function(e) {
        message("Network error for ID ", id, ": ", conditionMessage(e))
        NULL
      }
    )

    if (is.null(ph_r)) {
      WrongIDs <- c(WrongIDs, id)
    } else if (ph_r$status_code == 200) {
      ph_data <- tryCatch(
        jsonlite::fromJSON(httr::content(ph_r, as = "text", encoding = "UTF-8")),
        error = function(e) {
          message("JSON parse error for ID ", id, ": ", conditionMessage(e))
          NULL
        }
      )

      if (!is.null(ph_data) && length(ph_data) > 0) {
        ph_data$ID       <- id
        ph_data$DateTime <- Sys.time()
        Physicians       <- dplyr::bind_rows(Physicians, ph_data)
        message("Scraped ID ", id, "  (total: ", nrow(Physicians), " records)")
      } else {
        WrongIDs <- c(WrongIDs, id)
      }
    } else {
      message("Request failed for ID ", id, " (status ", ph_r$status_code, ")")
      WrongIDs <- c(WrongIDs, id)
    }

    Sys.sleep(delay_sec)
  }

  if (!is.null(output_dir)) {
    timestamp       <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
    ext             <- if (identical(output_format, "parquet")) ".parquet" else ".csv"
    physicians_file <- file.path(output_dir, paste0("Physicians_", startID, "-", endID, "_", timestamp, ext))
    wrong_tail      <- if (length(WrongIDs)) tail(WrongIDs, 1) else endID
    wrong_ids_file  <- file.path(output_dir, paste0("Wrong_IDs_", startID, "-", wrong_tail, "_", timestamp, ext))

    mysterycall_write_table(Physicians, physicians_file, format = output_format)
    mysterycall_write_table(data.frame(WrongIDs = WrongIDs), wrong_ids_file, format = output_format)

    message("Roster saved to: ", physicians_file)
    message("Wrong ID log saved to: ", wrong_ids_file)
  }

  if (isTRUE(interactive()) && requireNamespace("beepr", quietly = TRUE)) beepr::beep(2)
  invisible(Physicians)
}
