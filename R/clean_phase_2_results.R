#' Rename columns based on substring matches
#'
#' This function searches through column names in a data frame for specified substrings and renames the first matching column to a new name provided by the user. It provides detailed logs for each operation, including the columns found and any renaming actions taken. If multiple columns match a substring, only the first is renamed, and a warning is issued.
#'
#' @param data A data frame whose columns need renaming.
#' @param target_strings A vector of substrings to search for within column names.
#' @param new_names A vector of new names corresponding to the target strings.
#' @param verbose Logical flag controlling progress messages.
#' @return A data frame with renamed columns.
#' @export
#' @import dplyr
#' @examples
#' df <- data.frame(
#'   doctor_info = 1:5,
#'   patient_contact_data = 6:10
#' )
#' # Renaming 'doctor_info' to 'physician_info'
#' df <- rename_columns_by_substring(df,
#'                                   target_strings = c("doctor"),
#'                                   new_names = c("physician_info"))
#' print(df)
#'
#' # More complex example with multiple renamings
#' df <- data.frame(
#'   doc_information = 1:5,
#'   patient_contact = 6:10,
#'   doctor_notes = 11:15
#' )
#' # Renaming 'doc_information' to 'doctor_info' and 'doctor_notes' to 'notes'
#' df <- rename_columns_by_substring(df,
#'                                   target_strings = c("doc_information", "doctor_notes"),
#'                                   new_names = c("doctor_info", "notes"))
#' print(df)
rename_columns_by_substring <- function(data, target_strings, new_names, verbose = TRUE) {
  # Initial checks and setup
  if (length(target_strings) != length(new_names)) {
    stop("target_strings and new_names must have the same length.")
  }

  workflow_log("Starting column standardisation...", verbose = verbose)
  # Loop over all target strings
  for (i in seq_along(target_strings)) {
    # Identify columns that contain the target string
    matches <- grepl(target_strings[i], names(data), ignore.case = TRUE)
    matched_cols <- names(data)[matches]

    # Detailed log of what matches were found
    if (any(matches)) {
      workflow_log(
        sprintf(
          "Found %d column(s) matching '%s': %s",
          sum(matches),
          target_strings[i],
          paste(matched_cols, collapse = ", ")
        ),
        verbose = verbose
      )
      # Warn if more than one match is found
      if (length(matched_cols) > 1) {
        warning(sprintf("Multiple columns match '%s'. Only the first (%s) will be renamed to '%s'.\n", target_strings[i], matched_cols[1], new_names[i]))
      }
      # Rename the first matching column
      names(data)[names(data) == matched_cols[1]] <- new_names[i]
    } else {
      warning(sprintf("No columns found containing '%s'.\n", target_strings[i]))
    }
  }

  workflow_log(
    sprintf("Column renaming complete. Updated columns: %s", paste(names(data), collapse = ", ")),
    verbose = verbose
  )
  return(data)
}

#' Clean and process Phase 2 data
#'
#' This function reads data from a file or data frame, cleans column names, and applies renaming based on specified criteria to facilitate data analysis. The function logs each step of the process, including data loading, column cleaning, and renaming for transparency.
#'
#' @param data_or_path Path to the data file or a data frame.
#' @param required_strings Vector of substrings for which to search in column names.
#' @param standard_names Vector of new names to apply to the matched columns.
#' @param verbose Logical flag controlling progress messages.
#' @return A data frame with processed data. The returned data frame has an
#'   `"output_path"` attribute pointing to the CSV written to disk.
#' @export
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr filter mutate
#' @importFrom janitor clean_names
#' @examples
#' # Assuming an input path to a CSV file
#' input_path <- "path_to_your_data.csv"
#' required_strings <- c("physician_information", "able_to_contact_office")
#' standard_names <- c("physician_info", "contact_office")
#' cleaned_data <- clean_phase_2_data(input_path, required_strings, standard_names)
#'
#' # Directly using a data frame
#' df <- data.frame(
#'   doc_info = 1:5,
#'   contact_data = 6:10
#' )
#' required_strings <- c("doc_info", "contact_data")
#' standard_names <- c("doctor_info", "patient_contact_info")
#' cleaned_df <- clean_phase_2_data(df, required_strings, standard_names)
#' print(cleaned_df)
clean_phase_2_data <- function(data_or_path, required_strings, standard_names, verbose = TRUE) {
  # Data loading and initial checks
  if (is.character(data_or_path)) {
    if (!file.exists(data_or_path)) {
      stop("File does not exist at the specified path: ", data_or_path)
    }
    data <- readr::read_csv(data_or_path, show_col_types = FALSE)
    workflow_log(sprintf("Data read from file at: %s", data_or_path), verbose = verbose)
  } else if (is.data.frame(data_or_path)) {
    data <- data_or_path
    workflow_log("Data loaded from provided dataframe.", verbose = verbose)
  } else {
    stop("Data input must be either a dataframe or a valid file path.")
  }

  # Clean and standardize column names
  data <- janitor::clean_names(data)
  workflow_log("Columns have been cleaned to snake case format.", verbose = verbose)

  # Apply the renaming function with detailed logging
  data <- rename_columns_by_substring(data, required_strings, standard_names, verbose = verbose)

  # Additional data processing
  workflow_log("Proceeding with additional data processing steps...", verbose = verbose)

  # Saving the cleaned data
  current_datetime <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  output_file_path <- paste0("cleaned_phase_2_data_", current_datetime, ".csv")
  readr::write_csv(data, output_file_path)
  workflow_log(sprintf("Cleaned data successfully saved to: %s", output_file_path), verbose = verbose)
  attr(data, "output_path") <- output_file_path

  return(data)
}

# Example usage
# required_strings <- c("physician_information", "able_to_contact_office", "are_we_including",
#                       "reason_for_exclusions", "appointment_date", "number_of_transfers",
#                       "call_time", "hold_time", "notes", "person_completing")
# standard_names <- c("physician_info", "contact_office", "included_in_study",
#                     "exclusion_reasons", "appt_date", "transfer_count",
#                     "call_duration", "hold_duration", "notes", "completed_by")

# Assuming an input path, you would run it like this:
# input_file <- "/path/to/your/data.csv"
# cleaned_data <- clean_phase_2_data(input_file, required_strings, standard_names)
