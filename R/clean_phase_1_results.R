#' Clean Phase 1 Results Data
#'
#' This function reads the Phase 1 results data file, performs various cleaning
#' and transformation operations, and prepares the data for further analysis.
#' It ensures all required fields are present and formats column names. Missing
#' NPI numbers are handled by generating a unique `random_id`.
#'
#' @param df A data frame containing the Phase 1 results data. Ensure that it
#' includes columns like 'for_redcap', 'id', 'names', 'practice_name', 'phone_number',
#' 'state_name', and optionally 'npi'. If 'npi' is missing or any of its values are NA,
#' a `random_id` is generated as a fallback.
#'
#' @return Invisible NULL; the function is used for its side effects of cleaning data
#' and outputting a CSV file with cleaned data.
#'
#' @examples
#' \dontrun{
#' library(tyler)
#' file_path <- "/path/to/your/input/file.xls"
#' df <- readxl::read_excel(file_path)  # Assuming use of readxl for Excel files
#' clean_phase_1_results(df)
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

#' @seealso tyler
clean_phase_1_results <- function(df) {
  if (!requireNamespace("dplyr", quietly = TRUE) ||
      !requireNamespace("janitor", quietly = TRUE) ||
      !requireNamespace("readr", quietly = TRUE) ||
      !requireNamespace("stringr", quietly = TRUE) ||
      !requireNamespace("humaniformat", quietly = TRUE)) {
    stop("Required packages are not installed. Please install them using install.packages().")
  }

  cat("Converting column types...\n")
  df <- readr::type_convert(df)  # Convert column types

  cat("Cleaning column names...\n")
  df <- janitor::clean_names(df, case = "snake")  # Clean column names

  cat("Checking required columns...\n")
  required_columns <- c("names", "practice_name", "phone_number", "state_name")
  if (!all(required_columns %in% names(df))) {
    stop("The following required columns are missing: ", paste(setdiff(required_columns, names(df)), collapse = ", "))
  }

  cat("Handling missing NPI numbers...\n")
  # Check if NPI column exists and handle missing values
  if ("npi" %in% names(df)) {
    df <- df %>%
      mutate(random_id = ifelse(is.na(npi), base::sample(1:9999999999, size = n(), replace = TRUE), npi))
  } else {
    df <- mutate(df, random_id = base::sample(1000000000:9999999999, size = n(), replace = TRUE))
  }

  if (nrow(df) > 0) {
    cat("Duplicating rows...\n")
    df <- bind_rows(df, df)  # Duplicate rows

    cat("Arranging rows by 'names'...\n")
    df <- arrange(df, names)  # Arrange rows by 'names'

    cat("Adding insurance and duplicating rows...\n")
    df <- mutate(df, insurance = rep(c("Blue Cross/Blue Shield", "Medicaid"), length.out = nrow(df)))  # Add insurance and duplicate rows

    cat("Adding a numbered 'id' column...\n")
    df <- mutate(df, id = row_number(),  # Ensures unique ID for each row
                 id_number = paste0("id:", id))  # Add a numbered 'id' column

    cat("Extracting last name and creating 'dr_name'...\n")
    df <- mutate(df,
                 last_name = humaniformat::last_name(names),
                 dr_name = paste("Dr.", last_name))  # Extract last name and create 'dr_name'

    cat("Identifying academic or private practice...\n")
    df <- mutate(df,
                 academic = ifelse(str_detect(practice_name, str_c(c("University", "Medical College"), collapse = "|")),
                                   "University", "Private Practice"))  # Identify academic or private practice

    cat("Uniting columns for REDCap upload...\n")
    df <- select(df, for_redcap = paste(id, dr_name, insurance, phone_number, state_name, random_id, academic, id_number, sep = ", "),
                 everything())  # Unite columns for REDCap upload
  } else {
    cat("No data to process.\n")
  }

  # Save the dataframe to a CSV file with date and time in the filename
  current_datetime <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  output_file <- file.path("/path/to/output/directory", paste0("clean_phase_1_results_", current_datetime, ".csv"))
  write_csv(df, output_file)
  cat("Saved cleaned Phase 1 results dataframe to", output_file, "\n")
}

# file_path <- "ortho_sports_med/data/phase1/Late_Phase_1_Mystery caller - Sports med Only.xlsx"
# df <- read_xls(file_path)
# clean_phase_1_results(df)
