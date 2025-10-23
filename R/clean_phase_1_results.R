#' Clean Phase 1 Results Data
#'
#' This helper normalises Phase 1 call sheets so they are ready for REDCap
#' uploads and caller workbook splitting. It ensures the input is a data frame,
#' standardises column names, generates stable identifiers, and stamps the
#' output with a timestamped filename.
#'
#' @param phase1_data A data frame containing the Phase 1 results data. Ensure
#'   that it includes columns like `names`, `practice_name`, `phone_number`,
#'   `state_name`, and optionally `npi`. Missing `npi` values receive a
#'   generated `random_id` so callers always have an identifier.
#' @param output_directory Directory where the cleaned Phase 1 CSV should be
#'   written. Defaults to `tempdir()`.
#' @param verbose Logical. If `TRUE`, progress messages are emitted during
#'   cleaning. Defaults to `TRUE`.
#' @param duplicate_rows Logical. If `TRUE`, each row in `phase1_data` is
#'   duplicated to retain the previous behaviour that paired insurance entries
#'   for each physician. Set to `FALSE` to keep the original number of rows.
#' @param id_seed Optional integer seed used when generating fallback random IDs
#'   so runs can be reproduced without permanently mutating the global RNG
#'   state.
#'
#' @return Invisibly returns the cleaned data frame with an `"output_path"`
#'   attribute containing the CSV that was written to disk.
#'
#' @examples
#' \dontrun{
#' library(tyler)
#' file_path <- "/path/to/your/input/file.xls"
#' phase1_data <- readxl::read_excel(file_path)  # Assuming use of readxl for Excel files
#' clean_phase_1_results(phase1_data)
#' }
#'
#' @importFrom dplyr arrange bind_rows coalesce mutate row_number select everything if_else
#' @importFrom janitor clean_names
#' @importFrom readr type_convert write_csv
#' @importFrom stringr str_detect
#' @importFrom humaniformat last_name
#' @importFrom rlang .data
#' @importFrom stats runif
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
                                  duplicate_rows = TRUE,
                                  id_seed = NULL) {
  assert_is_dataframe(phase1_data, "phase1_data")
  ensure_required_packages()

  with_preserved_seed(id_seed, {
    workflow_log("Converting column types...", verbose = verbose)
    phase1_data <- readr::type_convert(phase1_data)

    workflow_log("Cleaning column names...", verbose = verbose)
    phase1_data <- janitor::clean_names(phase1_data, case = "snake")

    required_columns <- c("names", "practice_name", "phone_number", "state_name")
    assert_has_columns(phase1_data, required_columns, "phase1_data")

    workflow_log("Handling missing NPI numbers...", verbose = verbose)
    phase1_data <- add_random_identifier(phase1_data)

    if (nrow(phase1_data) > 0) {
      phase1_data <- maybe_duplicate_rows(phase1_data, duplicate_rows, verbose)
      phase1_data <- add_phase1_identifiers(phase1_data, verbose)
    } else {
      workflow_log("No data to process.", verbose = verbose)
    }

    output_file <- write_timestamped_phase1_csv(phase1_data, output_directory)
    attr(phase1_data, "output_path") <- output_file
    workflow_log(sprintf("Saved cleaned Phase 1 results dataframe to %s", output_file), verbose = verbose)
  })

  invisible(phase1_data)
}


ensure_required_packages <- function() {
  pkgs <- c("dplyr", "janitor", "readr", "stringr", "humaniformat")
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop(
      sprintf(
        "Required packages are not installed: %s",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(NULL)
}

with_preserved_seed <- function(seed, code) {
  if (is.null(seed)) {
    return(force(code))
  }

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

  set.seed(seed)
  force(code)
}

add_random_identifier <- function(data) {
  id_source <- if ("npi" %in% names(data)) data$npi else rep(NA_real_, nrow(data))
  needs_id <- is.na(id_source)
  replacements <- generate_random_ids(sum(needs_id))
  id_source[needs_id] <- replacements
  dplyr::mutate(data, random_id = id_source)
}

generate_random_ids <- function(n) {
  if (!n) {
    return(numeric(0))
  }
  floor(stats::runif(n, min = 1e9, max = 1e10))
}

maybe_duplicate_rows <- function(data, duplicate_rows, verbose) {
  if (isTRUE(duplicate_rows)) {
    workflow_log("Duplicating rows...", verbose = verbose)
    dplyr::bind_rows(data, data)
  } else {
    workflow_log("Skipping row duplication as requested...", verbose = verbose)
    data
  }
}

add_phase1_identifiers <- function(data, verbose) {
  workflow_log("Arranging rows by 'names'...", verbose = verbose)
  data <- dplyr::arrange(data, names)

  workflow_log("Adding insurance information...", verbose = verbose)
  data <- dplyr::mutate(
    data,
    insurance = rep(c("Blue Cross/Blue Shield", "Medicaid"), length.out = nrow(data))
  )

  workflow_log("Adding numbered identifiers...", verbose = verbose)
  data <- dplyr::mutate(
    data,
    id = dplyr::row_number(),
    id_number = paste0("id:", id)
  )

  workflow_log("Extracting last name and creating 'dr_name'...", verbose = verbose)
  data <- dplyr::mutate(
    data,
    last_name = humaniformat::last_name(names),
    dr_name = paste("Dr.", last_name)
  )

  workflow_log("Identifying academic or private practice...", verbose = verbose)
  data <- dplyr::mutate(
    data,
    academic = dplyr::if_else(
      stringr::str_detect(
        practice_name,
        stringr::str_c(c("University", "Medical College"), collapse = "|")
      ),
      "University",
      "Private Practice"
    )
  )

  workflow_log("Uniting columns for REDCap upload...", verbose = verbose)
  if ("npi" %in% names(data)) {
    data <- dplyr::mutate(
      data,
      doctor_id = dplyr::coalesce(as.character(.data$npi), as.character(random_id))
    )
  } else {
    data <- dplyr::mutate(data, doctor_id = as.character(random_id))
  }

  data <- dplyr::mutate(
    data,
    for_redcap = paste(
      id,
      dr_name,
      insurance,
      phone_number,
      state_name,
      random_id,
      academic,
      id_number,
      sep = ", "
    )
  )

  dplyr::select(data, for_redcap, dplyr::everything())
}

write_timestamped_phase1_csv <- function(data, output_directory) {
  if (!dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  }
  current_datetime <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  output_file <- file.path(output_directory, paste0("clean_phase_1_results_", current_datetime, ".csv"))
  readr::write_csv(data, output_file)
  output_file
}
