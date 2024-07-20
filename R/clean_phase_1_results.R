#' Clean Phase 1 Results Data
#'
#' This function reads the Phase 1 results data file, performs various cleaning and transformation operations,
#' and prepares the data for further analysis, including assigning lab assistants.
#'
#' @param df A data frame containing the Phase 1 results data.
#' @return None
#'
#' @examples
#' \dontrun{
#' library(tyler)
#' file_path <- "/path/to/your/input/file.xls"
#' df <- read_xls(file_path)
#' clean_phase_1_results(df)
#' }
#' @importFrom dplyr arrange pull filter mutate select bind_rows
#' @importFrom janitor clean_names
#' @importFrom readr type_convert write_csv
#' @importFrom stringr str_detect
#' @importFrom humaniformat last_name first_name
#' @export
#'

clean_phase_1_results <- function(df) {
  if (!requireNamespace("dplyr", quietly = TRUE) ||
      !requireNamespace("janitor", quietly = TRUE) ||
      !requireNamespace("readr", quietly = TRUE) ||
      !requireNamespace("stringr", quietly = TRUE) ||
      !requireNamespace("humaniformat", quietly = TRUE)) {
    stop("Required packages are not installed. Please install them using install.packages().")
  }

  cat("Converting column types...\n")
  df <- readr::type_convert(df) # Convert column types

  cat("Filtering out rows with missing 'npi'...\n")
  df <- dplyr::filter(df, !is.na(npi)) # Remove rows with missing 'npi'

  cat("Cleaning column names...\n")
  df <- janitor::clean_names(df, case = "snake") # Clean column names

  cat("Checking required columns...\n")
  required_columns <- c("names", "practice_name", "phone_number", "state_name")
  if (!all(required_columns %in% names(df))) {
    stop("The following required columns are missing: ", paste(setdiff(required_columns, names(df)), collapse = ", "))
  }

  if (nrow(df) > 0) {
    cat("Duplicating rows...\n")
    df <- dplyr::bind_rows(df, df) # Duplicate rows

    cat("Arranging rows by 'names'...\n")
    df <- dplyr::arrange(df, names) # Arrange rows by 'names'

    cat("Adding insurance and duplicating rows...\n")
    df <- dplyr::mutate(df, insurance = rep(c("Blue Cross/Blue Shield", "Medicaid"), length.out = nrow(df))) # Add insurance and duplicate rows

    cat("Adding a numbered 'id' column...\n")
    df <- dplyr::mutate(df, id = 1:nrow(df))
    df <- dplyr::mutate(df, id_number = paste0("id:", id)) # Add a numbered 'id' column

    cat("Extracting last name and creating 'dr_name'...\n")
    df <- dplyr::mutate(df,
                        last_name = humaniformat::last_name(names),
                        dr_name = paste("Dr.", last_name)) # Extract last name and create 'dr_name'

    cat("Identifying academic or private practice...\n")
    academic_keywords <- c("Medical College", "University of", "University", "Univ", "Children's", "Infirmary",
                           "Medical School", "Medical Center", "Children", "Health System", "Foundation",
                           "Sch of Med", "Dept of Oto", "Mayo", "UAB", "OTO Dept", "Cancer Ctr", "Penn",
                           "College of Medicine", "Cancer", "Cleveland Clinic", "Henry Ford", "Yale",
                           "Brigham", "Dept of OTO", "Health Sciences Center", "SUNY")
    df <- dplyr::mutate(df,
                        academic = ifelse(stringr::str_detect(practice_name, stringr::str_c(academic_keywords, collapse = "|", sep = "\\b|\\b", fixed = TRUE)),
                                          "University", "Private Practice")) # Identify academic or private practice

    cat("Uniting columns for REDCap upload...\n")
    df <- dplyr::mutate(df,
                        for_redcap = paste(id, dr_name, insurance, phone_number, state_name, npi, academic, id_number, sep = ", ")) %>%
      dplyr::select(for_redcap, id, phone_number, academic, everything()) # Unite columns for REDCap upload
  } else {
    cat("No data to process.\n")
  }

  # Save the dataframe to a CSV file with date and time in the filename
  current_datetime <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  output_file <- file.path("/Users/tylermuffly/Dropbox (Personal)/Mystery shopper/mystery_shopper/obgyn/data/phase2",
                           paste0("clean_phase_1_results_", current_datetime, ".csv"))
  readr::write_csv(df, output_file)
  cat("Saved cleaned Phase 1 results dataframe to", output_file, "\n")

  cat("You're awesome! Data is now ready for assigning lab assistants to each person!\n")
}
