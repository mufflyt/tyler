

## Geocode
create_geocode <- memoise::memoise(function(csv_file) {
  # Set your HERE API key
  api_key <- "VnDX-Rafqchcmb4LUDgEpYlvk8S1-LCYkkrtb1ujOrM"
  hereR::set_key(api_key)
  
  
  
  # Check if the CSV file exists
  if (!file.exists(csv_file)) {
    stop("CSV file not found.")
  }
  
  
  
  # Read the CSV file into a data frame
  data <- read.csv(csv_file)
  
  
  
  # Check if the data frame contains a column named "address"
  if (!"address" %in% colnames(data)) {
    stop("The CSV file must have a column named 'address' for geocoding.")
  }
  
  
  
  # Initialize a list to store geocoded results
  geocoded_results <- list()
  
  
  
  # Initialize progress bar
  pb <- progress_bar$new(total = nrow(data), format = "[:bar] :percent :elapsed :eta :rate")
  
  
  
  # Loop through each address and geocode it
  for (i in 1:nrow(data)) {
    address <- data[i, "address"]
    result <- hereR::geocode(address)
    geocoded_results[[i]] <- result
    cat("Geocoded address ", i, " of ", nrow(data), "\n")
    pb$tick() # Increment the progress bar
  }
  
  
  
  # Combine all geocoded results into one sf object
  geocoded <- do.call(rbind, geocoded_results)
  
  
  
  # Add the geocoded information to the original data frame
  data$latitude <- geocoded$latitude
  data$longitude <- geocoded$longitude
  
  
  
  # Write the updated data frame with geocoded information back to a CSV file
  write.csv(data, csv_file, row.names = FALSE)
  cat("Updated CSV file with geocoded information.\n")
  
  
  
  cat("Geocoding complete.\n")
  
  
  
  return(geocoded)
})
# Example usage:
library(progress)
library(sf)

csv_file <- "short_complete_npi_for_subspecialists.csv"
output_file <- "geocoded_addresses.csv"
geocoded_data <- create_geocode(csv_file)
View(geocoded_data)


st_write(geocoded_data, "geocoded_addresses.csv", layer_options = "GEOMETRY=AS_XY", append = T)
#write.csv(geocoded_data, "geocoded_addresses.csv")
###############################################################################
###############################################################################

library(dplyr)
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

  #location <- location %>%
    #dplyr::distinct(location)

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


create_isochrones_for_dataframe <- function(input_file, breaks = c(30*60, 60*60, 120*60, 180*60)) {

  Sys.setenv(HERE_API_KEY = "VnDX-Rafqchcmb4LUDgEpYlvk8S1-LCYkkrtb1ujOrM")
  readRenviron("~/.Renviron")
  hereR::set_key("VnDX-Rafqchcmb4LUDgEpYlvk8S1-LCYkkrtb1ujOrM")

  dataframe <- easyr::read.any(input_file) %>%
    filter(!is.na(Y) | !is.na(X))


  # Check if "lat" and "long" columns exist
  if (!all(c("Y", "X") %in% colnames(dataframe))) {
    stop("The dataframe must have 'lat' and 'long' columns.")
  }

  # Convert dataframe to sf object
  dataframe_sf <- dataframe %>%
    janitor::clean_names()%>%
    sf::st_as_sf(coords = c("x", "y"), crs = 4326)

  # Ensure it's an sf object
  if (!is(dataframe_sf, "sf")) {
    stop("FYI: The file is not an sf object.")
  }
  #dataframe_sf$geometry = st_as_sf(as.data.frame(dataframe_sf), coords = c("x","y"))
  class(dataframe_sf) #for testing
  dataframe <- dataframe_sf

  # Initialize isochrones as an empty data frame
  isochrones <- list()
  isochrones_temp <- list()

  # Loop over the rows in the dataframe
  for (i in 1:nrow(dataframe)) {
    print(i)

    # Get the point for the current row
    point_temp <- dataframe[i,]

    # Get isochrones for that point
    Sys.sleep(0.4)
    isochrones_temp[[i]] <- create_isochrones(location = point_temp, range = breaks)
    if (!is.null(isochrones_temp[[i]])) {
      # Flatten the list of isolines
      isochrones_temp[[i]] <- dplyr::bind_rows(isochrones_temp[[i]], .id = "column_label")

      # Create the 'name' column with descriptive labels
      isochrones_temp[[i]]$name <- cut(
        isochrones_temp[[i]]$range / 60,
        breaks = breaks,
        labels = paste0(head(breaks, -1), "-", tail(breaks, -1) - 1, " minutes")
      )


    }

  }
  isochrones <- data.table::rbindlist(isochrones_temp, fill = T)
  return(isochrones)
}
# Usage example:
# geocoded_addresses.csv

input_file <- "geocoded_addresses.csv"
isochrones_data <- create_isochrones_for_dataframe(input_file, breaks = c(1800, 3600, 7200, 10800))
