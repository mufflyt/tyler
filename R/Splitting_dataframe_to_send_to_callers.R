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

split_and_save <- function(data_or_path, output_directory, lab_assistant_names,
                                    seed = 1978,
                                    complete_file_prefix = "complete_non_split_version_",
                                    split_file_prefix = "",
                                    recursive_create = TRUE) {

  # Read the data from file if path provided
  if (is.data.frame(data_or_path)) {
    data <- data_or_path
  } else {
    # Read the data from file
    tryCatch({
      data <- easyr::read_any(data_or_path)
    }, error = function(e) {
      stop("Error reading the input file. Check if the file path and format are correct.")
    })
  }

  # Check if lab_assistant_names is provided and has at least two names
  if (length(lab_assistant_names) <= 1) {
    stop("Please provide at least two lab assistant names for the splits.")
  }

  # Check required columns exist
  required_columns <- c("for_redcap", "id")
  missing_columns <- required_columns[!required_columns %in% names(data)]
  if (length(missing_columns) > 0) {
    stop(paste("The input data is missing the following columns:", paste(missing_columns, collapse = ", ")))
  }

  # Randomize the data by 'id' column
  set.seed(seed)
  data <- dplyr::arrange(data, dplyr::sample_n(data, size = nrow(data)))

  # Add lab assistant assignment column
  data <- dplyr::mutate(data,
                        lab_assistant_assigned = rep(lab_assistant_names, length.out = nrow(data))) %>%
    dplyr::select(for_redcap, lab_assistant_assigned, everything())

  # Create output directory if it doesn't exist
  if (!fs::dir_exists(output_directory)) {
    tryCatch({
      fs::dir_create(output_directory, recursive = recursive_create)
    }, error = function(e) {
      stop("Failed to create output directory. Check directory permissions and try again.")
    })
  }

  # Save the complete data to a separate Excel file
  current_datetime <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  complete_output_file <- file.path(output_directory, paste0(complete_file_prefix, current_datetime, ".xlsx"))

  tryCatch({
    if (file.exists(complete_output_file)) {
      warning("Output file already exists. Skipping save to prevent overwrite.")
    } else {
      openxlsx::write.xlsx(data, complete_output_file)
      message("Saved unsplit and complete data to: ", complete_output_file)
    }
  }, error = function(e) {
    stop("Error saving the complete file. Check if the output directory is writable.")
  })

  # Split the data into parts based on lab assistants
  splits <- base::split(data, data$lab_assistant_assigned)

  # Save each split to a separate Excel file
  for (lab_assistant_name in base::names(splits)) {
    lab_assistant_data <- splits[[lab_assistant_name]]

    output_file <- file.path(output_directory,
                             paste0(split_file_prefix, lab_assistant_name, "_", current_datetime, "_", nrow(lab_assistant_data), ".xlsx"))

    tryCatch({
      if (file.exists(output_file)) {
        warning(paste("Output file", output_file, "already exists. Skipping save to prevent overwrite."))
      } else {
        openxlsx::write.xlsx(lab_assistant_data, output_file)
        message("Saved split data for", lab_assistant_name, "to:", output_file)
      }
    }, error = function(e) {
      warning(paste("Error saving split data for", lab_assistant_name, ". Continuing with other splits."))
    })
  }

  message("Each of the lab assistant's split files have been saved successfully!")
  message("Output directory:", output_directory)
}
