#' This function takes an input data file, splits it into multiple parts, and saves each part as a separate Excel file.
#'
#' @param input_path The path to the input data file (RDS, CSV, or XLS/XLSX).
#' @param output_directory The directory where the output Excel files will be saved.
#' @param lab_assistant_names A vector of lab_assistant_names to name the output files.
#' @return None
#' @export
#'
#' @import dplyr
#' @import readr
#' @import openxlsx
#' @import easyr
#'
#' @examples
#' \dontrun{
#' library(tyler)
#' input_path <- "/path/to/your/input/file.rds"
#' output_directory <- "/path/to/your/output/directory"
#' lab_assistant_names <- c("Label1", "Label2", "Label3")
#' split_and_save(input_path, output_directory, lab_assistant_names)
#' }
# Split the final graph.
# I have a dataframe of 1224 rows.  I want to keep the file in the order it is in now.   I need to split it eight ways and then send a CSV of each split to a person.
split_and_save <- function(input_path, output_directory, lab_assistant_names) {

  # Check if the number of lab_assistant_names matches the required number of splits
  n_splits <- length(lab_assistant_names)
  if (n_splits <= 1) {
    stop("Please provide at least two lab_assistant_names for the splits.")
  }

  sample_data <- easyr::read.any(input_path)

  cat("Checking for 'id' column...\n")

  # Check if the data contains a column named "id"
  if (!"id" %in% names(sample_data)) {
    stop("The input data does not contain a column named 'id'. Please make sure the column exists.")
  }

  cat("Randomizing the data by 'id' column...\n")

  # Randomize the data by 'id' column
  set.seed(1978) #in case we have to resplit and send out results again.
  sample_data <- sample_data %>%
    dplyr::arrange(sample(id))  # Shuffle the data

  cat("Adding 'lab_assistant_assigned_to_call' column...\n")

  # Add the 'lab_assistant_assigned_to_call' column based on the lab_assistant_names
  sample_data <- sample_data %>%
    dplyr::mutate(lab_assistant_assigned_to_call = rep(lab_assistant_names, length.out = nrow(sample_data))) %>%
    dplyr::select(for_redcap, lab_assistant_assigned_to_call, everything())

  cat("Saving the complete file before splitting...\n")

  # Save the complete data to a separate Excel file
  current_datetime <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  complete_output_file <- paste0(output_directory, "/complete_non_split_version_", current_datetime, ".xlsx")
  openxlsx::write.xlsx(sample_data, complete_output_file)
  cat("Saved unsplit and complete data to", complete_output_file, "\n")

  cat("Splitting data and saving to separate Excel files...\n")

  # Split the data into n_splits parts based on lab assistants
  splits <- split(x = sample_data, f = sample_data$lab_assistant_assigned_to_call)

  # Save each split to a separate Excel file
  for (lab_assistant_name in names(splits)) {
    # Extract data for the current lab assistant
    lab_assistant_data <- splits[[lab_assistant_name]]

    # Create a filename for the output Excel file
    output_file <- paste0(output_directory, "/", lab_assistant_name, "_", current_datetime, "_", nrow(lab_assistant_data)
                          , ".xlsx")

    # Write the lab assistant's data to the output Excel file
    openxlsx::write.xlsx(lab_assistant_data, output_file)
  }

  cat("Each of the lab assistant's split files have been saved successfully!\n")
  cat("Output directory:", output_directory, "\n")
}
