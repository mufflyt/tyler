#' Load Matching Data for a Residency Program
#'
#' This function reads and processes matching data for a given residency program and year.
#'
#' @param location The location of the residency program.
#' @param year The year of the matching data.
#'
#' @return A processed data frame containing the matching data, or NULL in case of an error.
#'
#' @importFrom exploratory read_delim_file clean_data_frame
#' @importFrom dplyr mutate filter case_when
#' @importFrom janitor clean_names
#' @importFrom lubridate mdy year
#'
#' @export
load_matching_data <- function(location, year) {
  # Testing
  # location <- "Utah"
  # year <- 2016

  # Log the start of the function
  message("Function Load Matching Data: Start")

  # Specify the path to the data folder
  data_folder <- "~/Dropbox (Personal)/Nomogram/nomogram/data/Archives/machine_readable/"

  # Construct the full path to the data file based on location and year
  pathandfilename <- paste0(data_folder, location, "_", year, ".csv")
  message("Function Load Matching Data: Reading data from", pathandfilename)

  tryCatch({
    # Attempt to read and process the data
    message("Function Load Matching Data: Attempting to read and process data")

    # Read in the data
    df <- exploratory::read_delim_file(pathandfilename, ",", quote = "\"", skip = 0,
                                       col_names = TRUE, na = c('', 'NA'),
                                       locale = readr::locale(encoding = "UTF-8", decimal_mark = ".", grouping_mark = ","),
                                       trim_ws = TRUE, progress = TRUE) %>%

      #df <- readr::read_csv("~/Dropbox (Personal)/Nomogram/nomogram/data/Archives/machine_readable/Utah_2016_test.csv")
      #df <- data.table::fread(pathandfilename)
      # Convert column types
      readr::type_convert() %>%
      # Clean the data frame
      exploratory::clean_data_frame() %>%
      # Add the 'Year' column
      dplyr::mutate(Year = year) %>%
      # Data cleaning and QA filters
      dplyr::filter(`Count of Peer Reviewed Book Chapter` != "Obstetrics-Gynecology|1076220C0 (Categorical)") %>%
      dplyr::filter(Gender %in% c("Female", "Male")) %>%
      # Format 'Date of Birth' column into date variable
      dplyr::mutate(`Date of Birth` = lubridate::mdy(`Date of Birth`)) %>%
      # Clean column names
      janitor::clean_names(case = "parsed") %>%
      # Add 'Date_of_Birth_year' column
      dplyr::mutate(Date_of_Birth_year = lubridate::year(Date_of_Birth)) %>%
      # Add 'Year_numeric' column and recode it
      dplyr::mutate(Year_numeric = as.numeric(Year)) %>%
      dplyr::mutate(Year_numeric = exploratory::recode(Year_numeric, `1` = 2017, `2` = 2018, `3` = 2019, `4` = 2020)) %>%
      # Calculate 'Age' column
      dplyr::mutate(Age = Year_numeric - Date_of_Birth_year) %>%
      # Add 'Location' column
      dplyr::mutate(Location = location) %>%
      # Convert selected columns to factors
      dplyr::mutate(dplyr::across(dplyr::where(is.character), as.factor))

    # Log successful data processing
    message("Function Load Matching Data: Data processed successfully")

  }, error = function(e) {
    # Log error message in case of an error
    message("Function Load Matching Data: Error -", e$message)
    df <- NULL
  })

  # Log the end of the function
  message("Function Load Matching Data: End")

  return(df)
}
