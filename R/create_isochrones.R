#' Memoized function to try a location with isoline calculations
#'
#' This function calculates isolines for a given location using the hereR package.
#'
#' @param location An sf object representing the location for which isolines will be calculated.
#' @param range A numeric vector of time ranges in seconds.
#' @param posix_time A POSIXct object representing the date and time of calculation. Default is "2023-10-20 08:00:00".
#'
#' @return A list of isolines for different time ranges, or an error message if the calculation fails.
#'
#' @examples
#' \dontrun{
#' # Set your HERE API key in your Renviron file using the following steps:
#' # 1. Add key to .Renviron
#' Sys.setenv(HERE_API_KEY = "your_api_key_here")
#' # 2. Reload .Renviron
#' readRenviron("~/.Renviron")
#'
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
#' @export
#' @importFrom memoise memoise
#' @importFrom hereR set_freemium set_key set_verbose isoline
create_isochrones <- memoise::memoise(function(location, range, posix_time = as.POSIXct("2023-10-20 08:00:00", format = "%Y-%m-%d %H:%M:%S")) {

  Sys.setenv(HERE_API_KEY = "VnDX-Rafqchcmb4LUDgEpYlvk8S1-LCYkkrtb1ujOrM")
  readRenviron("~/.Renviron")
  hereR::set_key("VnDX-Rafqchcmb4LUDgEpYlvk8S1-LCYkkrtb1ujOrM")


  cat("\033[Display setup instructions:\033[0m\n")
  cat("\033[34mTo create isochrones for a specific point(s) use the following code:\033[0m\n")
  cat("\033[34mtryLocationMemo(location = location, range = c(1800, 3600, 7200, 10800))\n")

  # # Check if location is an sf object
  # if (!base::inherits(location, "sf")) {
  #   stop("Location must be an sf object.")
  # }

  # Check if HERE_API_KEY is set in Renviron
  if (Sys.getenv("HERE_API_KEY") == "") {
    cat("Please set your HERE API key in your Renviron file using the following steps:\n")
    cat("1. Add key to .Renviron\n")
    cat("Sys.setenv(HERE_API_KEY = \"your_api_key_here\")\n")
    cat("2. Reload .Renviron\n")
    cat("readRenviron(\"~/.Renviron\")\n")
    stop("HERE_API_KEY environment variable is not set. Please set it to your HERE API key.")
  }

  # Initialize HERE API securely using an environment variable for the API key
  cat("Setting up the hereR access...\n")
  api_key <- Sys.getenv("HERE_API_KEY")

  hereR::set_freemium(ans = FALSE)
  hereR::set_key("VnDX-Rafqchcmb4LUDgEpYlvk8S1-LCYkkrtb1ujOrM")
  hereR::set_verbose(TRUE)

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
      cat("Isoline successfully produced for range:", r, "seconds\n")

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
    cat("Error in tryLocationMemo:", e$message, "\n")

    # Return an error message as a list
    return(list(error = e$message))
  })

  # Return the result, whether it's isolines or an error message
  return(out)
  cat("\tryLocation complete.\n")
})
