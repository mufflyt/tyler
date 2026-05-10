#' Memoized function to try a location with isoline calculations
#'
#' This function calculates isolines for a given location using a drive-time routing package.
#'
#' @param location An sf object representing the location for which isolines will be calculated.
#' @param range A numeric vector of time ranges in seconds.
#' @param posix_time A POSIXct object representing the date and time of calculation. Default is "2023-10-20 08:00:00".
#' @param api_key API key for the drive-time routing service. Defaults to the `HERE_API_KEY` environment variable.
#'
#' @return A named list of sf isolines keyed by range in seconds, or a list
#'   with an `error` element if the calculation fails.
#'
#' @examplesIf interactive()
#' # Set your routing API key in your Renviron file using the following steps:
#' # 1. Add key to .Renviron
#' # 2. Reload .Renviron
#'
#' # Define a sf object for the location
#' location <- sf::st_point(c(-73.987, 40.757))
#'
#' # Calculate isolines for the location with a 30-minute, 60-minute, 120-minute, and 180-minute range
#' isolines <- mysterycall_create_isochrones(location = location, range = c(1800, 3600, 7200, 10800))
#'
#' # Print the isolines
#' print(isolines)
#'
#' # Free the in-memory cache when done with a batch
#' mysterycall_clear_isochrone_cache()
#'
#' @family mapping
#' @export
#' @importFrom dplyr mutate row_number
mysterycall_create_isochrones <- function(location,
                              range,
                              posix_time = as.POSIXct("2023-10-20 08:00:00",
                                                      format = "%Y-%m-%d %H:%M:%S"),
                              api_key = Sys.getenv("HERE_API_KEY")) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required. Install with: install.packages('sf')", call. = FALSE)
  }

  if (!requireNamespace("hereR", quietly = TRUE)) {
    stop("Package 'hereR' is required for mysterycall_create_isochrones(). Install with: install.packages('hereR')", call. = FALSE)
  }
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required for mysterycall_create_isochrones(). Install with: install.packages('sf')", call. = FALSE)
  }
  if (!requireNamespace("lwgeom", quietly = TRUE)) {
    stop("Package 'lwgeom' is required for mysterycall_create_isochrones(). Install with: install.packages('lwgeom')", call. = FALSE)
  }

  if (is.null(.isochrone_memo)) {
    .isochrone_worker(location, range, posix_time, api_key)
  } else {
    .isochrone_memo(location, range, posix_time, api_key)
  }
}

# Internal worker function (unmemoized)
.isochrone_worker <- function(location, range, posix_time, api_key) {
  if (is.na(api_key) || !nzchar(api_key)) {
    stop("routing API key is required via argument or HERE_API_KEY env var.", call. = FALSE)
  }

  hereR::set_freemium(ans = FALSE)
  hereR::set_key(api_key)
  hereR::set_verbose(TRUE)

  isolines_list <- list()

  out <- tryCatch({
    for (r in range) {
      temp <- hereR::isoline(
        poi = location,
        datetime = posix_time,
        routing_mode = "fast",
        range = r,
        range_type = "time",
        transport_mode = "car",
        url_only = FALSE,
        optimize = "balanced",
        traffic = TRUE,
        aggregate = FALSE
      )

      message("Isoline successfully produced for range: ", r, " seconds")

      temp <- temp %>%
        sf::st_make_valid() %>%
        dplyr::mutate(unique_id = dplyr::row_number())

      if (!all(sf::st_is_valid(temp))) {
        stop("drive-time routing service returned geometries that could not be validated.", call. = FALSE)
      }

      temp <- sf::st_transform(temp, 4326)
      temp <- lwgeom::st_force_polygon_cw(temp)

      isolines_list[[as.character(r)]] <- temp
    }

    return(isolines_list)
  }, error = function(e) {
    message("Error in mysterycall_create_isochrones: ", e$message)
    return(list(error = e$message))
  })

  out
}

# Internal memoized worker - stored in the package namespace so
# mysterycall_clear_isochrone_cache() can call memoise::forget() on it.
# If memoise is not installed, .isochrone_memo is NULL and mysterycall_create_isochrones()
# falls back to calling .isochrone_worker() directly.
.isochrone_memo <- if (requireNamespace("memoise", quietly = TRUE)) {
  memoise::memoise(.isochrone_worker)
} else {
  NULL
}

#' Clear the isochrone memoization cache
#'
#' The [mysterycall_create_isochrones()] function caches every result in memory for the
#' duration of the R session. Call this after processing a large batch to
#' release that memory.
#'
#' @return Invisibly `NULL`.
#' @examples
#' mysterycall_clear_isochrone_cache()
#' @export
mysterycall_clear_isochrone_cache <- function() {
  if (!is.null(.isochrone_memo) && requireNamespace("memoise", quietly = TRUE)) {
    memoise::forget(.isochrone_memo)
  }
  invisible(NULL)
}
