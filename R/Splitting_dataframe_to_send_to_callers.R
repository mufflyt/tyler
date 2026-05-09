#' Split data into multiple parts and save each part as separate Excel files
#'
#' This function splits the data based on provided lab assistant names and saves each part as a separate Excel file.
#' It allows the arrangement of calls by insurance type to prioritize Medicaid in the first two days and Blue Cross/Blue Shield in the last two days.
#' @name mysterycall_split_and_save
#' @param data_or_path Either a dataframe containing the input data or a path to the input data file (RDS, CSV, Parquet, or XLS/XLSX).
#' @param output_directory Directory where output Excel files will be saved.
#' @param lab_assistant_names Vector of lab assistant names to name the output files.
#' @param seed Seed value for randomization (default is 1978).
#' @param complete_file_prefix Prefix for the complete output file name (default is "complete_non_split_version_").
#' @param split_file_prefix Prefix for each split output file name (default is empty).
#' @param recursive_create Logical indicating if directories should be created recursively (default is TRUE).
#' @param insurance_order Vector of insurance types ordered by priority for call scheduling (default is c("Medicaid", "Blue Cross/Blue Shield")).
#'
#' @importFrom dplyr arrange mutate group_by ungroup n
#' @importFrom readr read_csv
#' @return Invisible list of file paths to the created Excel files
#' @family workflow
#' @export
#'
#' @examples
#' \dontrun{
#' input_data <- readr::read_csv("/path/to/your/input/file.csv")
#' output_directory <- "/path/to/your/output/directory"
#' lab_assistant_names <- c("Label1", "Label2", "Label3")
#' insurance_order <- c("Medicaid", "Blue Cross/Blue Shield")
#' mysterycall_split_and_save(
#'   data_or_path = input_data,
#'   output_directory = output_directory,
#'   lab_assistant_names = lab_assistant_names,
#'   insurance_order = insurance_order
#' )
#' }

mysterycall_split_and_save <- function(data_or_path, output_directory, lab_assistant_names, seed = 1978,
                           complete_file_prefix = "complete_non_split_version_", split_file_prefix = "",
                           recursive_create = TRUE, insurance_order = c("Medicaid", "Blue Cross/Blue Shield")) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required for this function. Install with: install.packages('openxlsx')", call. = FALSE)
  }
  # Validate input data or read from file path
  if (is.character(data_or_path)) {
    if (!base::file.exists(data_or_path)) {
      stop("File does not exist at the specified path: ", data_or_path, call. = FALSE)
    }
    ext <- tolower(tools::file_ext(data_or_path))
    if (ext %in% c("csv", "parquet")) {
      data <- mysterycall_read_table(data_or_path)
    } else if (ext %in% c("rds", "rda")) {
      data <- readRDS(data_or_path)
    } else if (ext %in% c("xls", "xlsx")) {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("Reading Excel workbooks requires the 'readxl' package. Install it with install.packages('readxl').", call. = FALSE)
      }
      data <- readxl::read_excel(data_or_path)
    } else {
      stop("Unsupported file extension: ", ext, ". Provide a CSV, Parquet, RDS, or Excel file.", call. = FALSE)
    }
  } else if (is.data.frame(data_or_path)) {
    data <- data_or_path
  } else {
    stop("Data input must be either a dataframe or a valid file path.", call. = FALSE)
  }

  # Check for the presence of necessary columns including 'insurance'
  required_columns <- c("for_redcap", "id", "doctor_id", "insurance")
  missing_columns <- base::setdiff(required_columns, base::names(data))
  if (length(missing_columns) > 0) {
    stop("The input data is missing the following columns: ", base::paste(missing_columns, collapse = ", "), call. = FALSE)
  }
  message(
    sprintf(
      "Validated %d required columns for splitting: %s.",
      length(required_columns),
      base::paste(required_columns, collapse = ", ")
    )
  )

  # Ensure that all insurance types in the data are covered by insurance_order
  data_insurance_values <- unique(data$insurance[!is.na(data$insurance)])
  missing_from_order <- setdiff(data_insurance_values, insurance_order)
  if (length(missing_from_order) > 0) {
    stop(sprintf(
      "Data contains insurance value(s) not in insurance_order: %s. Add them to insurance_order.",
      paste(missing_from_order, collapse = ", ")
    ), call. = FALSE)
  }
  # Also warn if insurance_order contains values absent from the data
  extra_in_order <- setdiff(insurance_order, data_insurance_values)
  if (length(extra_in_order) > 0) {
    warning(sprintf(
      "insurance_order contains value(s) not found in the data: %s.",
      paste(extra_in_order, collapse = ", ")
    ), call. = FALSE)
  }

  # Create a ranking based on the insurance order for sorting
  insurance_rank <- setNames(seq_along(insurance_order), insurance_order)
  data <- data %>%
    dplyr::mutate(insurance_rank = insurance_rank[.data$insurance]) %>%
    dplyr::arrange(.data$insurance_rank, .data$doctor_id)  # Sort by insurance rank, then by doctor_id if necessary
  message(
    sprintf(
      "Arranged %d row(s) by insurance priority: %s.",
      nrow(data),
      paste(insurance_order, collapse = ", ")
    )
  )

  # Check if lab_assistant_names is provided and has at least two names
  if (length(lab_assistant_names) < 2) {
    stop("Please provide at least two lab assistant names for the splits.", call. = FALSE)
  }
  message(
    sprintf(
      "Preparing to split workbooks across %d lab assistant(s): %s.",
      length(lab_assistant_names),
      paste(lab_assistant_names, collapse = ", ")
    )
  )

  # Randomize the data within each insurance group
  set.seed(seed)
  if (nrow(data) == 0) {
    message("Input contains zero rows; workbooks will be created without assignments.")
    data <- data %>%
      dplyr::mutate(lab_assistant_assigned = character(dplyr::n()))
  } else {
    data <- data %>%
      dplyr::group_by(.data$insurance_rank) %>%
      dplyr::mutate(lab_assistant_assigned = sample(lab_assistant_names, dplyr::n(), replace = TRUE)) %>%
      dplyr::ungroup()
  }

  # Create output directory if it doesn't exist
  if (!dir.exists(output_directory)) {
    tryCatch({
      dir.create(output_directory, recursive = recursive_create, showWarnings = FALSE)
    }, error = function(e) {
      stop("Failed to create output directory. Check directory permissions and try again.", call. = FALSE)
    })
  }

  # Save the complete data to a separate Excel file
  current_datetime <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  complete_output_file <- file.path(output_directory, paste0(complete_file_prefix, current_datetime, ".xlsx"))

  tryCatch({
    openxlsx::write.xlsx(data, complete_output_file)
    message(sprintf("Saved unsplit roster (%d row(s)) to: %s", nrow(data), complete_output_file))
  }, error = function(e) {
    stop("Error saving the complete file. Check if the output directory is writable.", call. = FALSE)
  })

  # Split the data into parts based on lab assistants and save each part
  splits <- base::split(data, data$lab_assistant_assigned)
  split_paths <- character(length(splits))
  for (i in seq_along(splits)) {
    lab_assistant_name <- names(splits)[[i]]
    output_file <- file.path(output_directory,
                             paste0(split_file_prefix, lab_assistant_name, "_", current_datetime, ".xlsx"))
    tryCatch({
      openxlsx::write.xlsx(splits[[lab_assistant_name]], output_file)
      message(sprintf(
        "Saved %d row(s) for %s to: %s",
        nrow(splits[[lab_assistant_name]]),
        lab_assistant_name,
        output_file
      ))
    }, error = function(e) {
      stop("Error saving split data for ", lab_assistant_name, ". Check if the output directory is writable.", call. = FALSE)
    })
    split_paths[[i]] <- output_file
  }
  message(sprintf("Split run complete: generated %d workbook(s).", length(splits)))
  if (isTRUE(interactive()) && requireNamespace("beepr", quietly = TRUE)) beepr::beep(2)
  invisible(c(complete_output_file, split_paths))
}
