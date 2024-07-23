#' Split data into multiple parts and save each part as separate Excel files.
#'
#' @param data_or_path Either a dataframe containing the input data or a path to the input data file (RDS, CSV, or XLS/XLSX).
#' @param output_directory Directory where output Excel files will be saved.
#' @param lab_assistant_names Vector of lab assistant names to name the output files.
#' @param seed Seed value for randomization (default is 1978).
#' @param complete_file_prefix Prefix for the complete output file name (default is "complete_non_split_version_").
#' @param split_file_prefix Prefix for each split output file name (default is empty).
#' @param recursive_create Logical indicating if directories should be created recursively (default is TRUE).
#'
#' @importFrom dplyr arrange sample_n mutate select
#' @importFrom openxlsx write.xlsx
#' @importFrom fs dir_create dir_exists
#' @export
#'
#' @examples
#' \dontrun{
#' library(tyler)
#' input_data <- readr::read_csv("/path/to/your/input/file.csv")
#' output_directory <- "/path/to/your/output/directory"
#' lab_assistant_names <- c("Label1", "Label2", "Label3")
#' split_and_save(data_or_path = input_data, output_directory, lab_assistant_names)
#' }
library(dplyr)
library(openxlsx)
library(fs)

split_and_save <- function(data_or_path, output_directory, lab_assistant_names, seed = 1978,
                           complete_file_prefix = "complete_non_split_version_", split_file_prefix = "",
                           recursive_create = TRUE) {
  # Validate input data or read from file path
  if (is.character(data_or_path)) {
    # Assuming the path is to a CSV file for simplicity
    if (!file.exists(data_or_path)) {
      stop("File does not exist at the specified path: ", data_or_path)
    }
    data <- read.csv(data_or_path)  # Update with appropriate file reading logic
  } else if (is.data.frame(data_or_path)) {
    data <- data_or_path
  } else {
    stop("Data input must be either a dataframe or a valid file path.")
  }

  # Check for the presence of necessary columns
  required_columns <- c("for_redcap", "id", "doctor_id")
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0) {
    stop("The input data is missing the following columns: ", paste(missing_columns, collapse = ", "))
  }

  # Check if lab_assistant_names is provided and has at least two names
  if (length(lab_assistant_names) < 2) {
    stop("Please provide at least two lab assistant names for the splits.")
  }

  # Randomize the data by 'doctor_id' column
  set.seed(seed)
  data <- data %>%
    arrange(sample_n(., size = n()))

  # Assign lab assistants to each doctor
  lab_assignments <- data %>%
    group_by(doctor_id) %>%
    mutate(lab_assistant_assigned = sample(lab_assistant_names, 1))

  # Create output directory if it doesn't exist
  if (!dir_exists(output_directory)) {
    tryCatch({
      dir_create(output_directory, recursive = recursive_create)
    }, error = function(e) {
      stop("Failed to create output directory. Check directory permissions and try again.")
    })
  }

  # Save the complete data to a separate Excel file
  current_datetime <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  complete_output_file <- file.path(output_directory, paste0(complete_file_prefix, current_datetime, ".xlsx"))

  tryCatch({
    write.xlsx(lab_assignments, complete_output_file)
    message("Saved unsplit and complete data to: ", complete_output_file)
  }, error = function(e) {
    stop("Error saving the complete file. Check if the output directory is writable.")
  })

  # Split the data into parts based on lab assistants and save each part
  splits <- split(lab_assignments, lab_assignments$lab_assistant_assigned)
  for (lab_assistant_name in names(splits)) {
    output_file <- file.path(output_directory,
                             paste0(split_file_prefix, lab_assistant_name, "_", current_datetime, ".xlsx"))
    tryCatch({
      write.xlsx(splits[[lab_assistant_name]], output_file)
      message("Saved split data for ", lab_assistant_name, " to: ", output_file)
    }, error = function(e) {
      stop("Error saving split data for ", lab_assistant_name, ". Check if the output directory is writable.")
    })
  }
}
