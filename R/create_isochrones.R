#' Memoized function to try a location with isoline calculations
#'
#' This function calculates isolines for a given location using the hereR package.
#'
#' @param location An sf object representing the location for which isolines will be calculated.
#' @param range A numeric vector of time ranges in seconds.
#' @param posix_time A POSIXct object representing the date and time of calculation. Default is "2023-10-20 08:00:00".
#' @param verbose Logical; if TRUE, prints status messages while running. Default is FALSE.
#'
#' @return A list of isolines for different time ranges, or an error message if the calculation fails.
#'
#' @examples
#' \dontrun{
#' # Set your HERE API key in your Renviron file using the following steps:
#' # 1. Add key to .Renviron
#' #' # 2. Reload .Renviron
#' #'
#' # Define a sf object for the location
#' location <- sf::st_point(c(-73.987, 40.757))
#'
#' # Calculate isolines for the location with a 30-minute, 60-minute, 120-minute, and 180-minute range
#' isolines <- create_isochrones(location = location, range = c(1800, 3600, 7200, 10800))
#'
#' # Print the isolines
#' print(isolines)
#'
#' }
#'
#' @family mapping
#' @export
#' @importFrom memoise memoise
#' @importFrom hereR set_freemium set_key set_verbose isoline
create_isochrones <- memoise::memoise(function(location, range, posix_time = as.POSIXct("2023-10-20 08:00:00", format = "%Y-%m-%d %H:%M:%S"), api_key = Sys.getenv("HERE_API_KEY"), verbose = FALSE) {

  if (isTRUE(verbose)) {
    message("Display setup instructions:")
    message("To create isochrones for a specific point(s) use the following code:")
    message("tryLocationMemo(location = location, range = c(1800, 3600, 7200, 10800))")
  }

  # # Check if location is an sf object
  # if (!base::inherits(location, "sf")) {
  #   stop("Location must be an sf object.")
  # }

  if (api_key == "") {
    stop("HERE API key is required via argument or HERE_API_KEY env var.")
  }

  hereR::set_freemium(ans = FALSE)
  hereR::set_key(api_key)
  hereR::set_verbose(isTRUE(verbose))


  # Initialize a list to store the isolines
  isolines_list <- list()

  # Try to calculate isolines for the given location
  out <- tryCatch({
    for (r in range) {
      # Calculate isolines using hereR::isoline function
      temp <- hereR::isoline(
        poi = location, #sf object
        datetime = posix_time, #POSIXct object, datetime for the departure
        routing_mode = "fast", #Try to route fastest route or "short"est route.
        range = r,  # Time range in seconds
        range_type = "time", # character of the isolines: "distance" or "time"
        transport_mode = "car", #specified for "car" transport instead of "truck" or "pedestrian"
        url_only = FALSE,
        optimize = "balanced",
        traffic = TRUE, # Includes real-time traffic
        aggregate = FALSE
      )

      # Log the successful calculation
      if (isTRUE(verbose)) {
        message("Isoline successfully produced for range: ", r, " seconds")
      }

      # Add a unique identifier to each row in isochrones_temp
      temp <- temp %>%
        mutate(unique_id = row_number())

      # Store the isoline in the list
      isolines_list[[as.character(r)]] <- temp
    }

    # Return the list of isolines
    return(isolines_list)
  }, error = function(e) {
    # Handle any errors that occur during the calculation
    if (isTRUE(verbose)) {
      message("Error in tryLocationMemo: ", e$message)
    }

    # Return an error message as a list
    return(list(error = e$message))
  })

  # Return the result, whether it's isolines or an error message
  return(out)
  if (isTRUE(verbose)) {
    message("tryLocation complete.")
  }
})
