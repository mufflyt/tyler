#' Rename columns based on substring matches
#'
#' This function searches through column names in a data frame for specified substrings and renames the first matching column to a new name provided by the user. It provides detailed logs for each operation, including the columns found and any renaming actions taken. If multiple columns match a substring, only the first is renamed, and a warning is issued.
#'
#' @param data A data frame whose columns need renaming.
#' @param target_strings A vector of substrings to search for within column names.
#' @param new_names A vector of new names corresponding to the target strings.
#' @return A data frame with renamed columns.
#' @export
#' @import dplyr
#' @importFrom stats setNames
#' @importFrom utils head
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
rename_columns_by_substring <- function(data, target_strings, new_names) {
  # Initial checks and setup
  if (length(target_strings) != length(new_names)) {
    stop("target_strings and new_names must have the same length.")
  }

  message("--- Starting to search and rename columns based on target substrings ---")
  # Loop over all target strings
  rename_log <- list()
  rename_index <- 0L
  for (i in seq_along(target_strings)) {
    # Identify columns that contain the target string
    matches <- grepl(target_strings[i], names(data), ignore.case = TRUE)
    matched_cols <- names(data)[matches]

    # Detailed log of what matches were found
    if (any(matches)) {
      message(sprintf(
        "Matched %d column(s) containing '%s': %s",
        sum(matches),
        target_strings[i],
        paste(matched_cols, collapse = ", ")
      ))
      # Warn if more than one match is found
      if (length(matched_cols) > 1) {
        warning(sprintf("Multiple columns contained '%s'. Renaming only '%s' to '%s'.", target_strings[i], matched_cols[1], new_names[i]))
      }
      # Rename the first matching column
      names(data)[names(data) == matched_cols[1]] <- new_names[i]
      rename_index <- rename_index + 1L
      rename_log[[rename_index]] <- data.frame(
        pattern = target_strings[i],
        renamed_from = matched_cols[1],
        renamed_to = new_names[i],
        stringsAsFactors = FALSE
      )
      message(sprintf(
        "Renamed '%s' to '%s'.",
        matched_cols[1],
        new_names[i]
      ))
    } else {
      warning(sprintf("No columns contained '%s'; nothing was renamed for this pattern.", target_strings[i]))
    }
    message("")  # Adding a blank line for better separation
  }

  message(sprintf(
    "--- Column renaming complete. Final column set: %s ---",
    paste(names(data), collapse = ", ")
  ))
  if (length(rename_log)) {
    attr(data, "rename_log") <- do.call(rbind, rename_log)
  }
  return(data)
}

#' Clean and process Phase 2 data
#'
#' This function reads data from a file or data frame, cleans column names, and applies renaming based on specified criteria to facilitate data analysis. The function logs each step of the process, including data loading, column cleaning, and renaming for transparency.
#'
#' @param data_or_path Path to the data file or a data frame.
#' @param required_strings Vector of substrings for which to search in column names.
#' @param standard_names Vector of new names to apply to the matched columns.
#' @param output_directory Directory where the cleaned dataset should be written.
#'   Defaults to a session-specific folder inside [tempdir()] when not provided.
#' @param output_format File format to use when persisting the cleaned dataset.
#'   Supported values are "csv" (default) and "parquet".
#' @return A data frame with processed data.
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
clean_phase_2_data <- function(
  data_or_path,
  required_strings,
  standard_names,
  output_directory = NULL,
  output_format = c("csv", "parquet")
) {
  output_format <- match.arg(output_format)
  # Data loading and initial checks
  if (is.character(data_or_path)) {
    if (!file.exists(data_or_path)) {
      stop("File does not exist at the specified path: ", data_or_path)
    }
    input_format <- tyler_normalize_file_format(path = data_or_path)
    data <- tyler_read_table(data_or_path, format = input_format)
    message(sprintf("Loaded Phase 2 data from %s with %d row(s) and %d column(s).", data_or_path, nrow(data), ncol(data)))
  } else if (is.data.frame(data_or_path)) {
    data <- data_or_path
    message(sprintf("Loaded Phase 2 data from provided data frame with %d row(s) and %d column(s).", nrow(data), ncol(data)))
  } else {
    stop("Data input must be either a dataframe or a valid file path.")
  }

  validate_dataframe(data, name = "phase2_data")

  if (missing(required_strings) || !length(required_strings)) {
    stop("`required_strings` must supply at least one pattern to search for.", call. = FALSE)
  }
  if (missing(standard_names) || !length(standard_names)) {
    stop("`standard_names` must supply at least one column name to apply.", call. = FALSE)
  }

  if (length(unique(standard_names)) != length(standard_names)) {
    warning("Duplicate values detected in `standard_names`; later entries may overwrite earlier renames.")
  }

  # Clean and standardize column names
  data <- janitor::clean_names(data)
  message("Converted column names to snake_case format.")

  # Apply the renaming function with detailed logging
  data <- rename_columns_by_substring(data, required_strings, standard_names)
  rename_log <- attr(data, "rename_log")
  if (!is.null(rename_log)) {
    message("Summary of applied renames:")
    summary_lines <- utils::capture.output(print(rename_log))
    message(paste(summary_lines, collapse = "\n"))
  }
  message("Standardised Phase 2 column names based on required patterns.")

  # Additional data processing
  message("Proceeding with additional data processing steps...")

  available_standard <- intersect(standard_names, names(data))
  if (length(available_standard)) {
    data <- dplyr::select(data, dplyr::all_of(available_standard), dplyr::everything())
  }

  # Saving the cleaned data
  if (is.null(output_directory)) {
    output_directory <- tyler_tempdir("phase2", create = TRUE)
  } else if (!dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  }

  current_datetime <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  output_extension <- if (identical(output_format, "parquet")) ".parquet" else ".csv"
  output_file_path <- file.path(output_directory, paste0("cleaned_phase_2_data_", current_datetime, output_extension))
  tyler_write_table(data, output_file_path, format = output_format)
  message(sprintf("Cleaned Phase 2 data (%d row(s), %d column(s)) saved to: %s", nrow(data), ncol(data), output_file_path))

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
