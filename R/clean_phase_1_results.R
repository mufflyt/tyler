#' Clean Phase 1 Results Data
#'
#' This function reads the Phase 1 results data file, performs various cleaning
#' and transformation operations, and prepares the data for further analysis.
#' It ensures all required fields are present and formats column names. Missing
#' NPI numbers are handled by generating a unique `random_id`.
#'
#' **New in Version 2.0: Data Provenance and Audit Trail**
#'
#' This function now includes comprehensive data provenance tracking:
#' - Preserves original `id` and `npi` columns as `original_id` and `original_npi`
#' - Adds processing flags (e.g., `processing_flag_empty_name`, `processing_flag_generated_id`)
#' - Handles edge cases like whitespace-only strings safely
#' - Saves audit trail JSON file with processing metadata
#' - Attaches audit trail and quality metrics as data frame attributes
#'
#' @param phase1_data A data frame containing the Phase 1 results data. Ensure that it
#' includes columns like 'for_redcap', 'id', 'names', 'practice_name', 'phone_number',
#' 'state_name', and optionally 'npi'. If 'npi' is missing or any of its values are NA,
#' a `random_id` is generated as a fallback. If an 'id' column exists, it will be
#' preserved as 'original_id'.
#' @param output_directory Directory where the cleaned Phase 1 data should be written.
#'   Defaults to `tempdir()`. An audit trail JSON file will also be saved here.
#' @param verbose Logical. If `TRUE`, progress messages are printed while cleaning.
#'   Defaults to `TRUE`.
#' @param notify Logical. If `TRUE`, play a notification sound on completion when
#'   the optional `beepr` package is available. Defaults to `TRUE`.
#' @param duplicate_rows Logical. If `TRUE`, each row in `phase1_data` is duplicated
#'   to retain the previous behaviour that paired insurance entries for each
#'   physician. Set to `FALSE` to keep the original number of rows. A
#'   `processing_flag_is_duplicate` column tracks which rows are duplicates.
#' @param id_seed Optional integer seed used when generating fallback random IDs so
#'   runs can be reproduced without permanently mutating the global RNG state.
#' @param output_format File format to use when writing the cleaned dataset.
#'   Supported options are "csv" (default) and "parquet".
#'
#' @return Invisibly returns the cleaned data frame with the following attributes:
#' \itemize{
#'   \item `audit_trail`: List containing processing metadata (timestamps, row counts, flags)
#'   \item `processing_timestamp`: Time when processing completed
#'   \item `function_version`: Version number of the function (2.0)
#'   \item `output_path`: Path to the saved CSV/Parquet file
#'   \item `audit_trail_path`: Path to the saved audit trail JSON file
#' }
#'
#' The returned data frame includes processing flag columns:
#' \itemize{
#'   \item `processing_flag_empty_name`: TRUE if name was empty/whitespace
#'   \item `processing_flag_generated_id`: TRUE if random ID was generated (no NPI)
#'   \item `processing_flag_is_duplicate`: TRUE for duplicated rows (when duplicate_rows=TRUE)
#'   \item `processing_flag_no_last_name`: TRUE if last name could not be extracted
#' }
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

clean_phase_1_results <- function(phase1_data,
                                  output_directory = tempdir(),
                                  verbose = TRUE,
                                  notify = TRUE,
                                  duplicate_rows = TRUE,
                                  id_seed = NULL,
                                  output_format = c("csv", "parquet")) {
  output_format <- match.arg(output_format)
  if (!requireNamespace("dplyr", quietly = TRUE) ||
      !requireNamespace("janitor", quietly = TRUE) ||
      !requireNamespace("readr", quietly = TRUE) ||
      !requireNamespace("stringr", quietly = TRUE) ||
      !requireNamespace("humaniformat", quietly = TRUE)) {
    stop("Required packages are not installed. Please install them using install.packages().")
  }

  announce <- function(...) {
    if (isTRUE(verbose)) {
      text <- paste0(...)
      text <- sub("\\n$", "", text)
      message(text)
    }
  }

  # === START: AUDIT TRAIL AND PROVENANCE ===
  # Capture processing start time and metadata
  processing_start_time <- Sys.time()
  audit_trail <- list(
    function_name = "clean_phase_1_results",
    start_time = processing_start_time,
    r_version = R.version.string,
    platform = .Platform$OS.type,
    package_version = tryCatch(as.character(packageVersion("tyler")), error = function(e) "unknown"),
    parameters = list(
      output_directory = output_directory,
      verbose = verbose,
      notify = notify,
      duplicate_rows = duplicate_rows,
      id_seed = id_seed,
      output_format = output_format
    ),
    input_rows = nrow(phase1_data),
    input_cols = ncol(phase1_data),
    input_colnames = names(phase1_data)
  )

  # Preserve original ID column if it exists
  original_id_preserved <- FALSE
  if ("id" %in% names(phase1_data)) {
    announce("Preserving original 'id' column as 'original_id'...")
    phase1_data <- dplyr::rename(phase1_data, original_id = id)
    original_id_preserved <- TRUE
    audit_trail$original_id_preserved <- TRUE
  }
  # === END: AUDIT TRAIL AND PROVENANCE ===

  validate_dataframe(phase1_data, name = "phase1_data")

  if (!is.null(id_seed)) {
    restore_rng <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      get(".Random.seed", envir = .GlobalEnv)
    } else {
      NULL
    }
    on.exit({
      if (is.null(restore_rng)) {
        if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
          rm(".Random.seed", envir = .GlobalEnv)
        }
      } else {
        assign(".Random.seed", restore_rng, envir = .GlobalEnv)
      }
    }, add = TRUE)
    set.seed(id_seed)
  }

  announce("Converting column types...")
  phase1_data <- readr::type_convert(phase1_data)

  announce("Cleaning column names...")
  phase1_data <- janitor::clean_names(phase1_data, case = "snake")

  announce("Checking required columns...")
  required_columns <- c("names", "practice_name", "phone_number", "state_name")
  validate_required_columns(phase1_data, required_columns, name = "phase1_data")
  announce(sprintf(
    "Validated %d required columns (%s).",
    length(required_columns),
    paste(required_columns, collapse = ", ")
  ))

  announce("Standardising core text fields...")
  phase1_data <- dplyr::mutate(
    phase1_data,
    names = stringr::str_squish(as.character(names)),
    practice_name = stringr::str_squish(as.character(practice_name)),
    phone_number = format_phone_number(phone_number),
    state_name = stringr::str_squish(as.character(state_name))
  )

  # === EDGE CASE HANDLING: Whitespace-only strings ===
  # Replace empty or whitespace-only names with NA to prevent humaniformat errors
  announce("Handling edge cases in name field...")
  phase1_data <- dplyr::mutate(
    phase1_data,
    names = ifelse(names == "" | is.na(names), NA_character_, names),
    processing_flag_empty_name = (is.na(names))
  )
  empty_names_count <- sum(phase1_data$processing_flag_empty_name, na.rm = TRUE)
  if (empty_names_count > 0) {
    announce(sprintf("Flagged %d row(s) with empty or whitespace-only names.", empty_names_count))
    audit_trail$empty_names_count <- empty_names_count
  }
  # === END: EDGE CASE HANDLING ===

  missing_phone <- sum(is.na(phase1_data$phone_number) | phase1_data$phone_number == "")
  if (missing_phone) {
    announce(sprintf("Detected %d row(s) with missing or unparseable phone numbers.", missing_phone))
  }

  announce("Handling missing NPI numbers...")

  # === NPI PRESERVATION: Preserve original NPI values ===
  original_npi_preserved <- FALSE
  if ("npi" %in% names(phase1_data)) {
    # Store original NPI for provenance tracking
    phase1_data <- dplyr::mutate(phase1_data, original_npi = npi)
    original_npi_preserved <- TRUE
    audit_trail$original_npi_preserved <- TRUE
    audit_trail$input_npi_count <- sum(!is.na(phase1_data$npi))
  }

  generate_random_ids <- function(n) {
    if (!n) {
      return(numeric(0))
    }
    # Use timestamp + process ID for better uniqueness
    base_time <- as.numeric(Sys.time())
    process_id <- Sys.getpid()
    unique_ids <- numeric(n)

    for (i in seq_len(n)) {
      # Combine timestamp, process ID, and counter to ensure uniqueness
      unique_ids[i] <- as.numeric(paste0(
        floor(base_time),
        sprintf("%05d", process_id %% 100000),
        sprintf("%03d", i)
      ))
    }
    unique_ids
  }

  if ("npi" %in% names(phase1_data)) {
    phase1_data <- dplyr::mutate(
      phase1_data,
      random_id = ifelse(
        is.na(npi),
        generate_random_ids(dplyr::n()),
        npi
      ),
      processing_flag_generated_id = is.na(npi)
    )
    generated_ids_count <- sum(phase1_data$processing_flag_generated_id, na.rm = TRUE)
    if (generated_ids_count > 0) {
      announce(sprintf("Generated %d random IDs for missing NPIs.", generated_ids_count))
      audit_trail$generated_ids_count <- generated_ids_count
    }
  } else {
    phase1_data <- dplyr::mutate(
      phase1_data,
      random_id = generate_random_ids(dplyr::n()),
      processing_flag_generated_id = TRUE
    )
    announce(sprintf("No NPI column found; generated %d random IDs.", nrow(phase1_data)))
    audit_trail$all_ids_generated <- TRUE
  }
  # === END: NPI PRESERVATION ===

  if (nrow(phase1_data) > 0) {
    # === ROW DUPLICATION TRACKING ===
    rows_before_duplication <- nrow(phase1_data)
    if (isTRUE(duplicate_rows)) {
      announce("Duplicating rows for insurance pairing...")
      phase1_data <- dplyr::bind_rows(phase1_data, phase1_data)
      # Add flag to track which rows are duplicates (second half)
      phase1_data <- dplyr::mutate(
        phase1_data,
        processing_flag_is_duplicate = rep(c(FALSE, TRUE), each = rows_before_duplication)
      )
      audit_trail$rows_duplicated <- TRUE
      audit_trail$rows_after_duplication <- nrow(phase1_data)
    } else {
      announce(sprintf(
        "Skipping row duplication as requested; keeping %d original row(s).",
        nrow(phase1_data)
      ))
      phase1_data <- dplyr::mutate(
        phase1_data,
        processing_flag_is_duplicate = FALSE
      )
      audit_trail$rows_duplicated <- FALSE
    }
    # === END: ROW DUPLICATION TRACKING ===

    announce("Arranging rows by 'names'...")
    phase1_data <- dplyr::arrange(phase1_data, names)

    announce("Adding insurance information...")
    phase1_data <- dplyr::mutate(
      phase1_data,
      insurance = rep(c("Blue Cross/Blue Shield", "Medicaid"), length.out = nrow(phase1_data))
    )

    announce("Adding a numbered 'id' column...")
    phase1_data <- dplyr::mutate(
      phase1_data,
      id = dplyr::row_number(),
      id_number = paste0("id:", id)
    )

    # === SAFE LAST NAME EXTRACTION: Handle NA and empty names ===
    announce("Extracting last name and creating 'dr_name'...")
    phase1_data <- dplyr::mutate(
      phase1_data,
      last_name = ifelse(
        is.na(names) | names == "",
        NA_character_,
        tryCatch(
          humaniformat::last_name(names),
          error = function(e) {
            warning(sprintf("Error extracting last name: %s. Returning NA.", e$message), call. = FALSE)
            NA_character_
          }
        )
      ),
      dr_name = ifelse(
        is.na(last_name),
        NA_character_,
        paste("Dr.", last_name)
      ),
      processing_flag_no_last_name = is.na(last_name)
    )
    no_last_name_count <- sum(phase1_data$processing_flag_no_last_name, na.rm = TRUE)
    if (no_last_name_count > 0) {
      announce(sprintf("Could not extract last name for %d row(s).", no_last_name_count))
      audit_trail$no_last_name_count <- no_last_name_count
    }
    # === END: SAFE LAST NAME EXTRACTION ===

    announce("Identifying academic or private practice...")
    phase1_data <- dplyr::mutate(
      phase1_data,
      academic = ifelse(
        stringr::str_detect(practice_name, stringr::str_c(c("University", "Medical College"), collapse = "|")),
        "University",
        "Private Practice"
      )
    )

    announce("Uniting columns for REDCap upload...")
    phase1_data <- dplyr::mutate(
      phase1_data,
      doctor_id = if ("npi" %in% names(phase1_data)) {
        dplyr::coalesce(as.character(npi), as.character(random_id))
      } else {
        as.character(random_id)
      },
      for_redcap = paste(id, dr_name, insurance, phone_number, state_name, random_id, academic, id_number, sep = ", ")
    )

    phase1_data <- dplyr::select(phase1_data, for_redcap, dplyr::everything())
    attr(phase1_data, "output_directory") <- output_directory
  } else {
    if (isTRUE(verbose)) {
      cat("No data to process.\n")
    }
    announce("No data available; skipping duplication, insurance assignment, and ID generation.")
  }

  # === FINALIZE AUDIT TRAIL AND PROVENANCE ===
  processing_end_time <- Sys.time()
  audit_trail$end_time <- processing_end_time
  audit_trail$duration_seconds <- as.numeric(difftime(processing_end_time, processing_start_time, units = "secs"))
  audit_trail$output_rows <- nrow(phase1_data)
  audit_trail$output_cols <- ncol(phase1_data)
  audit_trail$rows_retained_pct <- if (audit_trail$input_rows > 0) {
    (audit_trail$output_rows / audit_trail$input_rows) * 100
  } else {
    0
  }

  # Add provenance metadata as attributes
  attr(phase1_data, "audit_trail") <- audit_trail
  attr(phase1_data, "processing_timestamp") <- processing_end_time
  attr(phase1_data, "function_version") <- "2.0"  # Version with provenance tracking

  # Calculate data quality metrics
  quality_metrics <- list(
    completeness_npi = if ("npi" %in% names(phase1_data)) sum(!is.na(phase1_data$npi)) / nrow(phase1_data) else 0,
    completeness_phone = if ("phone_number" %in% names(phase1_data)) sum(!is.na(phase1_data$phone_number) & phase1_data$phone_number != "") / nrow(phase1_data) else 0,
    completeness_names = if ("names" %in% names(phase1_data)) sum(!is.na(phase1_data$names) & phase1_data$names != "") / nrow(phase1_data) else 0,
    has_processing_flags = any(grepl("^processing_flag_", names(phase1_data)))
  )
  audit_trail$quality_metrics <- quality_metrics

  announce(sprintf(
    "Processing complete: %d → %d rows (%.1f%%) in %.2f seconds",
    audit_trail$input_rows,
    audit_trail$output_rows,
    audit_trail$rows_retained_pct,
    audit_trail$duration_seconds
  ))
  # === END: FINALIZE AUDIT TRAIL ===

  current_datetime <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  if (!dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  }
  output_extension <- if (identical(output_format, "parquet")) ".parquet" else ".csv"
  output_file <- file.path(output_directory, paste0("clean_phase_1_results_", current_datetime, output_extension))
  tyler_write_table(phase1_data, output_file, format = output_format)
  announce(sprintf(
    "Saved cleaned Phase 1 results dataframe to %s with %d row(s) and %d column(s).",
    output_file,
    nrow(phase1_data),
    ncol(phase1_data)
  ))
  attr(phase1_data, "output_path") <- output_file

  # === SAVE AUDIT TRAIL ===
  # Save audit trail as separate JSON file for data provenance
  audit_file <- file.path(output_directory, paste0("audit_trail_", current_datetime, ".json"))
  tryCatch({
    if (requireNamespace("jsonlite", quietly = TRUE)) {
      jsonlite::write_json(audit_trail, audit_file, pretty = TRUE, auto_unbox = TRUE)
      announce(sprintf("Saved audit trail to %s", audit_file))
      attr(phase1_data, "audit_trail_path") <- audit_file
    }
  }, error = function(e) {
    warning(sprintf("Could not save audit trail: %s", e$message), call. = FALSE)
  })
  # === END: SAVE AUDIT TRAIL ===

  if (isTRUE(notify) && requireNamespace("beepr", quietly = TRUE)) {
    beepr::beep(2)
  }

  invisible(phase1_data)
}


format_phone_number <- function(phone_values) {
  phone_chr <- as.character(phone_values)
  digits <- gsub("[^0-9]", "", phone_chr)

  strip_leading_one <- function(x) {
    if (nchar(x) == 11 && substr(x, 1, 1) == "1") {
      substr(x, 2, 11)
    } else {
      x
    }
  }

  formatted <- vapply(digits, function(d) {
    if (is.na(d) || !nzchar(d)) {
      return("")
    }
    clean <- strip_leading_one(d)
    if (nchar(clean) == 10) {
      sprintf("(%s) %s-%s", substr(clean, 1, 3), substr(clean, 4, 6), substr(clean, 7, 10))
    } else if (nchar(clean) == 7) {
      sprintf("%s-%s", substr(clean, 1, 3), substr(clean, 4, 7))
    } else if (nchar(clean) == 0) {
      ""
    } else {
      # Bug #5 fix: Warn about invalid phone number lengths instead of silently returning malformed numbers
      warning(sprintf(
        "Phone number has invalid length (%d digits): '%s'. Expected 7 or 10 digits. Returning as-is.",
        nchar(clean), d
      ), call. = FALSE)
      clean
    }
  }, character(1))

  # Preserve original NAs
  formatted[is.na(phone_chr)] <- NA_character_

  formatted
}


# file_path <- "ortho_sports_med/data/phase1/Late_Phase_1_Mystery caller - Sports med Only.xlsx"
# phase1_data <- read_xls(file_path)
# clean_phase_1_results(phase1_data)
