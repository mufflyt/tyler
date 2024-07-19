#' Test and Process Isochrones
#'
#' This function tests and processes isochrones for each location in the input file. It
#' identifies and reports any errors encountered during the isochrone retrieval process.
#'
#' @param input_file A data frame containing location data with columns "lat" and "long."
#'                  The input file should represent geographic coordinates for which
#'                  isochrones will be calculated.
#'
#' @return Prints messages indicating errors, if any, during isochrone retrieval.
#'
#' @details This function uses the `hereR` package to calculate isochrones based on the
#'          provided geographic coordinates. It retrieves isochrones for each location in
#'          the input file, identifies any errors during the retrieval process, and reports
#'          these errors. The function is designed to be used with input data that meets
#'          specific requirements, including valid latitude and longitude values.
#'
#' @importFrom dplyr mutate filter row_number
#' @importFrom sf st_as_sf
#' @importFrom hereR isoline set_key
#'
#' @examples
#' # Validate the file of geocoded data.
#' input_file <- readr::read_csv("data/isochrones/inner_join_postmastr_clinician_data.csv") %>%
#'   dplyr::mutate(id = dplyr::row_number()) %>%
#'   dplyr::filter(postmastr.name.x != "Hye In Park, MD")
#'
#' test_and_process_isochrones(input_file = input_file)
#'
#' # Filter out the rows that are going to error out after using the test_and_process_isochrones function.
#' # error_rows <- c(265, 431, 816, 922, 1605, 2049, 2212, 2284, 2308, 2409, 2482, 2735, 2875, 2880, 3150, 3552, 3718)
#' # input_file_no_error_rows <- input_file %>%
#' #   dplyr::filter(!id %in% error_rows)
#'

test_and_process_isochrones <- function(input_file) {
  # Parameter validation
  stopifnot(is.data.frame(input_file), all(c("lat", "long") %in% colnames(input_file)))

  Sys.setenv(HERE_API_KEY = "VnDX-Rafqchcmb4LUDgEpYlvk8S1-LCYkkrtb1ujOrM")
  readRenviron("~/.Renviron")
  hereR::set_key("VnDX-Rafqchcmb4LUDgEpYlvk8S1-LCYkkrtb1ujOrM")

  input_file <- input_file %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    dplyr::filter(postmastr.name.x != "Hye In Park, MD")

  input_file$lat <- as.numeric(input_file$lat)
  input_file$long <- as.numeric(input_file$long)

  input_file_sf <- input_file %>%
    sf::st_as_sf(coords = c("long", "lat"), crs = 4326)

  posix_time <- as.POSIXct("2023-10-20 09:00:00", format = "%Y-%m-%d %H:%M:%S")

  error_rows <- vector("list", length = nrow(input_file_sf))

  for (i in 1:nrow(input_file_sf)) {
    row_data <- input_file_sf[i, ]

    isochrones <- tryCatch(
      {
        hereR::isoline(
          poi = row_data,
          range = c(1),
          datetime = posix_time,
          routing_mode = "fast",
          range_type = "time",
          transport_mode = "car",
          url_only = FALSE,
          optimize = "balanced",
          traffic = TRUE,
          aggregate = FALSE
        )
      },
      error = function(e) {
        message("Error processing row ", i, ": ", e$message)
        return(NULL)
      }
    )

    if (is.null(isochrones)) {
      error_rows[[i]] <- i
    }
  }

  # Collect the rows that caused errors
  error_rows <- unlist(error_rows, use.names = FALSE)

  if (length(error_rows) > 0) {
    message("Rows with errors: ", paste(error_rows, collapse = ", "))
  } else {
    message("No errors found.")
  }
}

#' Process and Save Isochrones
#'
#' This function takes an input file of locations, retrieves isochrones for each location,
#' and saves them as shapefiles. It processes the data in chunks of 25 rows at a time to
#' prevent data loss in case of errors.
#'
#' @param input_file A data frame containing location data with columns "lat" and "long."
#'                  The input file should represent geographic coordinates for which
#'                  isochrones will be calculated.
#' @param chunk_size The number of rows to process in each chunk. Default is 25.
#'
#' @return An sf (simple features) data frame containing isochrone polygons.
#'
#' @details This function uses the `hereR` package to calculate isochrones based on the
#'          provided geographic coordinates. It retrieves isochrones for each location in
#'          the input file, processes the data in chunks to minimize the risk of data loss
#'          in case of errors, and saves the isochrones as shapefiles for further analysis.
#'
#' @importFrom dplyr mutate filter row_number
#' @importFrom sf st_as_sf st_write
#' @importFrom hereR isoline set_key
#'
#' @examples
#' # Load the input file (e.g., from a CSV)
#' input_file <- read_csv("data/locations.csv")
#'
#' # Process and save isochrones for the input file (chunk size set to 25)
#' isochrones_data <- process_and_save_isochrones(input_file, chunk_size = 25)
#'
#' # Optionally, write the combined isochrones to a shapefile
#' sf::st_write(isochrones_data, dsn = "data/isochrones/isochrones_all_combined",
#'              layer = "isochrones", driver = "ESRI Shapefile", quiet = FALSE)
#'

process_and_save_isochrones <- function(input_file, chunk_size = 25) {
  # Parameter validation
  stopifnot(is.data.frame(input_file), all(c("lat", "long") %in% colnames(input_file)),
            is.numeric(chunk_size), chunk_size > 0)

  Sys.setenv(HERE_API_KEY = "VnDX-Rafqchcmb4LUDgEpYlvk8S1-LCYkkrtb1ujOrM")
  readRenviron("~/.Renviron")
  hereR::set_key("VnDX-Rafqchcmb4LUDgEpYlvk8S1-LCYkkrtb1ujOrM")

  input_file <- input_file %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    dplyr::filter(postmastr.name.x != "Hye In Park, MD")

  input_file$lat <- as.numeric(input_file$lat)
  input_file$long <- as.numeric(input_file$long)

  input_file_sf <- input_file %>%
    sf::st_as_sf(coords = c("long", "lat"), crs = 4326)

  posix_time <- as.POSIXct("2023-10-20 09:00:00", format = "%Y-%m-%d %H:%M:%S")

  num_chunks <- ceiling(nrow(input_file_sf) / chunk_size)
  isochrones_list <- list()

  for (i in 1:num_chunks) {
    start_idx <- (i - 1) * chunk_size + 1
    end_idx <- min(i * chunk_size, nrow(input_file_sf))
    chunk_data <- input_file_sf[start_idx:end_idx, ]

    isochrones <- tryCatch(
      {
        hereR::isoline(
          poi = chunk_data,
          range = c(1800, 3600, 7200, 10800),
          datetime = posix_time,
          routing_mode = "fast",
          range_type = "time",
          transport_mode = "car",
          url_only = FALSE,
          optimize = "balanced",
          traffic = TRUE,
          aggregate = FALSE
        )
      },
      error = function(e) {
        message("Error processing chunk ", i, ": ", e$message)
        return(NULL)
      }
    )

    if (!is.null(isochrones)) {
      # Create the file name with the current date and time
      current_datetime <- format(Sys.time(), "%Y%m%d%H%M%S")
      file_name <- paste("data/isochrones/isochrones_", current_datetime, "_chunk_", i)

      # Assuming "arrival" field is originally in character format with both date and time
      # Convert it to a DateTime object
      isochrones$arrival <- as.POSIXct(isochrones$arrival, format = "%Y-%m-%d %H:%M:%S")

      # Save the data as a shapefile with the layer name "isochrones"
      sf::st_write(
        isochrones,
        dsn = file_name,
        layer = "isochrones",
        driver = "ESRI Shapefile",
        quiet = FALSE
      )

      # Store the isochrones in the list
      isochrones_list[[i]] <- isochrones
    }
  }

  # Combine all isochrones from the list into one data frame
  isochrones_data <- do.call(rbind, isochrones_list)

  return(isochrones_data)
}
