#' Scrape Physicians' Data with Tor
#'
#' `r lifecycle::badge("deprecated")`
#'
#' This helper previously proxied credentialed API requests through Tor while the
#' package was under heavy development. The implementation was noisy, relied on
#' interactive credentials, and created temporary files as a side effect. The
#' functionality is no longer maintained and has therefore been deprecated.
#'
#' @param startID Ignored. Retained for backward compatibility.
#' @param endID Ignored. Retained for backward compatibility.
#' @param torPort Ignored. Retained for backward compatibility.
#' @param wrong_ids_path Ignored. Included for compatibility with earlier
#'   releases.
#' @return A zero-row tibble that preserves representative column names for
#'   downstream code that still expects a data frame.
#' @details The original scraping logic has been removed. Instead the function
#'   raises a lifecycle deprecation warning and returns an empty tibble so that
#'   dependent code can handle the absence of data gracefully.
#' @deprecated This debugging utility has been deprecated and will be removed in
#'   a future release.
#' @export
scrape_physicians_data_with_tor <- function(startID, endID, torPort, wrong_ids_path = NULL) {
  lifecycle::deprecate_warn(
    when = "1.2.2",
    what = "tyler::scrape_physicians_data_with_tor()",
    details = "The debugging helper no longer performs network requests."
  )

  tibble::tibble(
    ID = integer(),
    DateTime = as.POSIXct(character()),
    startID = numeric(),
    endID = numeric(),
    torPort = numeric()
  )
}

# Example usage with Tor port 9150 (update with your Tor port if different)
# scrape_physicians_data_with_tor(startID = 9045923, endID = 9055923, torPort = 9150)
