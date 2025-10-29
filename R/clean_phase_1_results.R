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
#' @param output_directory Directory where the cleaned Phase 1 data should be written.
#'   Defaults to `tempdir()`.
#' @param verbose Logical. If `TRUE`, progress messages are printed while cleaning.
#'   Defaults to `TRUE`.
#' @param notify Logical. If `TRUE`, play a notification sound on completion when
#'   the optional `beepr` package is available. Defaults to `TRUE`.
#' @param duplicate_rows Logical. If `TRUE`, each row in `phase1_data` is duplicated
#'   to retain the previous behaviour that paired insurance entries for each
#'   physician. Set to `FALSE` to keep the original number of rows.
#' @param id_seed Optional integer seed used when generating fallback random IDs so
#'   runs can be reproduced without permanently mutating the global RNG state.
#' @param output_format File format to use when writing the cleaned dataset.
#'   Supported options are "csv" (default) and "parquet".
#'
#' @return Invisibly returns the cleaned data frame.
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

  missing_phone <- sum(is.na(phase1_data$phone_number) | phase1_data$phone_number == "")
  if (missing_phone) {
    announce(sprintf("Detected %d row(s) with missing or unparseable phone numbers.", missing_phone))
  }

  announce("Handling missing NPI numbers...")
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
      )
    )
  } else {
    phase1_data <- dplyr::mutate(
      phase1_data,
      random_id = generate_random_ids(dplyr::n())
    )
  }

  if (nrow(phase1_data) > 0) {
    if (isTRUE(duplicate_rows)) {
      announce("Duplicating rows...")
      phase1_data <- dplyr::bind_rows(phase1_data, phase1_data)
    } else {
      announce(sprintf(
        "Skipping row duplication as requested; keeping %d original row(s).",
        nrow(phase1_data)
      ))
    }

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

    announce("Extracting last name and creating 'dr_name'...")
    phase1_data <- dplyr::mutate(
      phase1_data,
      last_name = humaniformat::last_name(names),
      dr_name = paste("Dr.", last_name)
    )

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
