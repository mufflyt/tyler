#' Validate and Remove Invalid NPI Numbers
#'
#' This function reads a CSV file containing NPI numbers, validates their
#' format using the npi package, and removes rows with missing or invalid NPIs.
#'
#' @param input_data Either a data frame containing NPI numbers or a path to a CSV file.
#'
#' @return A data frame containing valid NPI numbers, with invalid or missing NPI rows removed.
#'
#' @section Contract:
#' **Inputs:**
#' - `input_data` must contain an `npi` column (character or numeric).
#' - NPIs are validated via the Luhn checksum algorithm (NPI standard).
#'
#' **Guarantees:**
#' - Output rows \eqn{\subseteq} input rows — rows are only removed, never added.
#' - Every removed row had either `NA`, a non-10-digit string, or a failed Luhn
#'   checksum.
#' - Output NPI column is always character type.
#'
#' **Disaster Prevention:**
#' Prevents silent retention of placeholder NPIs (e.g., all-zeros or
#' test values such as "1234567890") that would match real providers in
#' downstream joins and inflate match rates.
#'
#' @section Performance:
#' O(n) with a small constant; Luhn validation is a pure arithmetic check.
#' Expect < 0.1 s for 10,000 rows.
#'
#' @section Called By:
#' - [mysterycall_run_workflow()]
#' - [mysterycall_search_and_process_npi()]
#'
#' @importFrom npi npi_is_valid
#' @importFrom readr read_csv cols col_character col_guess
#' @importFrom dplyr filter mutate
#' @family data-quality
#' @export
#' @examplesIf interactive()
#' df <- data.frame(npi = c("1234567893", "0000000000", NA_character_))
#' mysterycall_validate_npi(df)
mysterycall_validate_npi <- function(input_data) {

  if (is.data.frame(input_data)) {
    npi_df <- input_data
  } else if (is.character(input_data) && length(input_data) == 1) {
    if (!file.exists(input_data)) {
      stop(sprintf("CSV file not found: %s", input_data), call. = FALSE)
    }

    npi_df <- readr::read_csv(
      input_data,
      col_types = readr::cols(.default = readr::col_guess(), npi = readr::col_character())
    )
  } else {
    stop(sprintf("`input_data` must be a data frame or a single CSV file path; received class: %s.", paste(class(input_data), collapse = ", ")), call. = FALSE)
  }

  if (!"npi" %in% names(npi_df)) {
    stop(sprintf("`input_data` is missing required column `npi`. Available columns: %s", if (length(names(npi_df))) paste(names(npi_df), collapse = ", ") else "<none>"), call. = FALSE)
  }

  npi_df <- npi_df %>%
    dplyr::mutate(
      npi = as.character(npi),
      npi = gsub("[^0-9]", "", npi),
      npi = trimws(npi)
    ) %>%
    dplyr::filter(!is.na(npi) & npi != "")

  total_candidates <- nrow(npi_df)

  if (!total_candidates) {
    npi_df$npi_is_valid <- logical()
    message("No NPI values remained after removing blanks; returning empty result.")
    return(npi_df[, unique(c("npi", "npi_is_valid", names(npi_df))), drop = FALSE])
  }

  valid_format <- nchar(npi_df$npi) == 10
  npi_df$npi_is_valid <- FALSE

  if (any(valid_format)) {
    npi_df$npi_is_valid[valid_format] <- vapply(
      npi_df$npi[valid_format],
      npi::npi_is_valid,
      logical(1)
    )
  }

  npi_df <- npi_df %>%
    dplyr::filter(!is.na(npi_is_valid) & npi_is_valid)

  message(sprintf(
    "Validated %d candidate NPI(s); %d passed checksum and formatting rules.",
    total_candidates,
    nrow(npi_df)
  ))

  npi_df
}
