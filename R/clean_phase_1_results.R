#' Clean Phase 1 Results Data
#'
#' This function reads the Phase 1 results data file, performs various cleaning and transformation operations,
#' and prepares the data for further analysis, including assigning lab assistants.
#'
#' @param file_path The path to the Phase 1 results data file (XLS/XLSX format).
#' @return None
#'
#' @examples
#' \dontrun{
#' library(tyler)
#' file_path <- "/path/to/your/input/file.xls"
#' clean_phase_1_results(file_path)
#' }
#' @import dplyr
#' @import readr
#' @import readxl
#' @import exploratory
#' @import humaniformat
#' @import janitor
#' @import stringr
#' @export
#'

# This should take all included physicians from phase 1 calling and then remove missing NPI numbers, clean the column names, duplicate the columns and assign an insurance type to them.  Lastly it brings all this data together into one column that can be uploaded to redcap.  At this point the lab assistants can call and find the wait times as they have the doctor name, state, phone number, etc.  Plus there is the NPI number so I can join it back to any other demographics that I may have from NPPES or elsewhere.

clean_phase_1_results <- function(file_path) {
  library(dplyr)
  library(readr)
  library(readxl)
  library(humaniformat)
  library(janitor)
  library(stringr)

  cat("Reading the .xls data file...\n")

  # Read data from different file formats (CSV, XLS/XLSX, or RDS)
  df <- read_xls(file_path)


  cat("Converting column types...\n")
  df <- df %>%
    readr::type_convert() # Convert column types

  cat("Filtering out rows with missing 'npi'...\n")
  df <- df %>%
    dplyr::filter(!is.na(npi)) # Remove rows with missing 'npi'

  cat("Cleaning column names...\n")
  df <- df %>%
    janitor::clean_names(case = "snake") # Clean column names

  cat("Checking required columns...\n")

  required_columns <- c("names", "practice_name", "phone_number", "state_name")
  if (!all(required_columns %in% names(df))) {
    stop("The following required columns are missing: ", paste(setdiff(required_columns, names(df)), collapse = ", "))
  }
  cat("Duplicating rows...\n")
  df <- df %>%
    exploratory::bind_rows(., .) # Duplicate rows

  cat("Arranging rows by 'names'...\n")
  df <- df %>%
    dplyr::arrange(names) # Arrange rows by 'names'

  cat("Adding insurance and duplicating rows...\n")
  df <- df %>%
    dplyr::mutate(insurance = rep(c("Blue Cross/Blue Shield", "Medicaid"), length.out = nrow(.))) # Add insurance and duplicate rows

  cat("Adding a numbered 'id' column...\n")
  df <- df %>% dplyr::mutate(id = 1:n())
  df <- df %>% dplyr::mutate(id_number = paste0("id:", id)) # Add a numbered 'id' column

  cat("Extracting last name and creating 'dr_name'...\n")
  df <- df %>%
    dplyr::mutate(
      last_name = humaniformat::last_name(names),
      dr_name = paste("Dr.", last_name)
    ) # Extract last name and create 'dr_name'

  cat("Identifying academic or private practice...\n")
  df <- df %>%
    dplyr::mutate(academic = ifelse(stringr::str_detect(practice_name, stringr::str_c(c("Medical College", "University of", "University", "Univ", "Children's", "Infirmary", "Medical School", "Medical Center", "Medical Center", "Children", "Health System", "Foundation", "Sch of Med", "Dept of Oto", "Mayo", "UAB", "OTO Dept", "Cancer Ctr", "Penn", "College of Medicine", "Cancer", "Cleveland Clinic", "Henry Ford", "Yale", "Brigham", "Dept of OTO", "Health Sciences Center", "SUNY"), collapse = "|", sep = "\\b|\\b", fixed = TRUE)), "University", "Private Practice")) # Identify academic or private practice

  cat("Uniting columns for REDCap upload...\n")
  # I included id twice because redcap requires the id at the start of the string but then does not show it.  Christ have mercy.
  df <- df %>%
    dplyr::mutate(for_redcap = paste(id, dr_name, insurance, phone_number, state_name, npi, academic, id_number, sep = ", ")) %>%
    dplyr::select(for_redcap, id, phone_number, academic, everything())
  # unite(for_redcap, dr_name, insurance, phone_number, state_name, npi, academic, sep = ", ", remove = FALSE, na.rm = FALSE)  # Unite columns for REDCap upload

  # Save the dataframe to a CSV file with date and time in the filename
  current_datetime <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  output_file <- paste0("/Users/tylermuffly/Dropbox (Personal)/Mystery shopper/mystery_shopper/obgyn/data/phase2/clean_phase_1_results_", current_datetime, ".csv")
  readr::write_csv(df, output_file)
  cat("Saved cleaned Phase 1 results dataframe to", output_file, "\n")

  cat("You're awesome! Data is now ready for assigning lab assistants to each person!\n")
}

# # Provide the path to the CSV file.  This is the ifnal phase 1 data from Hannah.
# file_path <- "/Users/tylermuffly/Dropbox (Personal)/Mystery shopper/mystery_shopper/obgyn/data/phase1_results/ACOG Calling List Phase 1 Final.xls"
#
# # Call the function and save the cleaned dataframe
# clean_phase_1_results(file_path)
