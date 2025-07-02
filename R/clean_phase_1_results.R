#' Clean Phase 1 Results Data
#'
#' This function reads the Phase 1 results data file, performs various cleaning
#' and transformation operations, and prepares the data for further analysis.
#' It ensures all required fields are present and formats column names. Missing
#' NPI numbers are handled by generating a unique `random_id`.
#'
#' @param phase1_data A data frame containing the Phase 1 results data. Ensure that it
#' includes columns like 'for_redcap', 'id', 'names', 'practice_name', 'phone_number',
#' 'state_name', and optionally 'npi'. If 'npi' is missing or any of its values are NA,
#' a `random_id` is generated as a fallback.
#'
#' @param output_dir Directory where the cleaned CSV will be saved. Defaults to
#'   the current working directory.
#' @return Invisible NULL; the function is used for its side effects of cleaning
#'   data and outputting a CSV file with cleaned data.
#'
#' @examples
#' \dontrun{
#' library(tyler)
#' file_path <- "/path/to/your/input/file.xls"
#' phase1_data <- readxl::read_excel(file_path)  # Assuming use of readxl for Excel files
#' clean_phase_1_results(phase1_data)
#' }
#'
#' @importFrom dplyr arrange mutate select filter bind_rows
#' @importFrom janitor clean_names
#' @importFrom readr type_convert write_csv
#' @importFrom stringr str_detect
#' @importFrom humaniformat last_name
#' @importFrom readxl read_excel
#' @importFrom purrr set_names
#' @export

# library(dplyr)
# library(janitor)
# library(readr)
# library(stringr)
# library(humaniformat)
# library(openxlsx)
# library(fs)

clean_phase_1_results <- function(phase1_data, output_dir = getwd()) {
  if (!requireNamespace("dplyr", quietly = TRUE) ||
      !requireNamespace("janitor", quietly = TRUE) ||
      !requireNamespace("readr", quietly = TRUE) ||
      !requireNamespace("stringr", quietly = TRUE) ||
      !requireNamespace("humaniformat", quietly = TRUE)) {
    stop("Required packages are not installed. Please install them using install.packages().")
  }

  cat("Converting column types...\n")
  phase1_data <- readr::type_convert(phase1_data)  # Convert column types

  cat("Cleaning column names...\n")
  phase1_data <- janitor::clean_names(phase1_data, case = "snake")  # Clean column names

  cat("Checking required columns...\n")
  required_columns <- c("names", "practice_name", "phone_number", "state_name")
  if (!all(required_columns %in% names(phase1_data))) {
    stop("The following required columns are missing: ", paste(setdiff(required_columns, names(phase1_data)), collapse = ", "))
  }

  cat("Handling missing NPI numbers...\n")
  # Check if NPI column exists and handle missing values
  if ("npi" %in% names(phase1_data)) {
    phase1_data <- phase1_data %>%
      mutate(random_id = ifelse(is.na(npi), base::sample(1:9999999999, size = n(), replace = TRUE), npi))
  } else {
    phase1_data <- mutate(phase1_data, random_id = base::sample(1000000000:9999999999, size = n(), replace = TRUE))
  }

  if (nrow(phase1_data) > 0) {
    cat("Duplicating rows...\n")
    phase1_data <- bind_rows(phase1_data, phase1_data)  # Duplicate rows

    cat("Arranging rows by 'names'...\n")
    phase1_data <- arrange(phase1_data, names)  # Arrange rows by 'names'

    cat("Adding insurance and duplicating rows...\n")
    phase1_data <- mutate(phase1_data, insurance = rep(c("Blue Cross/Blue Shield", "Medicaid"), length.out = nrow(phase1_data)))  # Add insurance and duplicate rows

    cat("Adding a numbered 'id' column...\n")
    phase1_data <- mutate(phase1_data, id = row_number(),  # Ensures unique ID for each row
                 id_number = paste0("id:", id))  # Add a numbered 'id' column

    cat("Extracting last name and creating 'dr_name'...\n")
    phase1_data <- mutate(phase1_data,
                 last_name = humaniformat::last_name(names),
                 dr_name = paste("Dr.", last_name))  # Extract last name and create 'dr_name'

    cat("Identifying academic or private practice...\n")
    phase1_data <- mutate(phase1_data,
                 academic = ifelse(str_detect(practice_name, str_c(c("University", "Medical College"), collapse = "|")),
                                   "University", "Private Practice"))  # Identify academic or private practice

    cat("Uniting columns for REDCap upload...\n")
    phase1_data <- select(phase1_data, for_redcap = paste(id, dr_name, insurance, phone_number, state_name, random_id, academic, id_number, sep = ", "),
                 everything())  # Unite columns for REDCap upload
  } else {
    cat("No data to process.\n")
  }

  # Save the dataframe to a CSV file with date and time in the filename
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  current_datetime <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  output_file <- file.path(output_dir, paste0("clean_phase_1_results_", current_datetime, ".csv"))
  write_csv(phase1_data, output_file)
  cat("Saved cleaned Phase 1 results dataframe to", output_file, "\n")
}

# file_path <- "ortho_sports_med/data/phase1/Late_Phase_1_Mystery caller - Sports med Only.xlsx"
# phase1_data <- read_xls(file_path)
# clean_phase_1_results(phase1_data)
