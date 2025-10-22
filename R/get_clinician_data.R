#' Retrieve Clinician Data
#'
#' This function retrieves clinician data for each valid NPI in the input dataframe.
#'
#' @param input_data Either a dataframe containing NPI numbers or a path to a CSV file.
#'
#' @return A tibble with clinician data for the provided NPIs.
#' @importFrom purrr map
#' @importFrom readr read_csv cols col_character col_guess
#' @importFrom dplyr mutate bind_rows
#' @family npi
#' @export
#' @examples
#' \dontrun{
#' clinician_df <- retrieve_clinician_data("clinicians.csv")
#' }
retrieve_clinician_data <- function(input_data) {
  if (!requireNamespace("provider", quietly = TRUE)) {
    stop(
      "Package 'provider' is required for this function. ",
      "Install it from GitHub with remotes::install_github('andrewallenbruce/provider').",
      call. = FALSE
    )
  }

  if (is.data.frame(input_data)) {
    clinician_df <- input_data
  } else if (is.character(input_data) && length(input_data) == 1) {
    clinician_df <- readr::read_csv(
      input_data,
      col_types = readr::cols(.default = readr::col_guess(), npi = readr::col_character())
    )
  } else {
    stop("Input must be a dataframe or a file path to a CSV.")
  }

  if (!"npi" %in% names(clinician_df)) {
    stop("Input data must contain an 'npi' column.")
  }

  cleaned_df <- validate_and_remove_invalid_npi(clinician_df)
  if (!nrow(cleaned_df)) {
    cleaned_df$npi_is_valid <- logical()
    return(cleaned_df)
  }

  get_clinician_data <- function(npi) {
    npi <- as.character(npi)
    if (nchar(npi) != 10 || grepl("\\D", npi)) {
      return(NULL)
    }

    clinician_info <- provider::clinicians(npi = npi)
    if (is.null(clinician_info) || !nrow(clinician_info)) {
      return(NULL)
    }
    as.data.frame(clinician_info, stringsAsFactors = FALSE)
  }

  cleaned_df <- cleaned_df %>%
    dplyr::mutate(clinician_data = purrr::map(npi, get_clinician_data))

  has_results <- vapply(cleaned_df$clinician_data, function(x) {
    !is.null(x) && nrow(x) > 0
  }, logical(1))

  base_cols <- setdiff(names(cleaned_df), "clinician_data")

  if (!any(has_results)) {
    return(cleaned_df[FALSE, base_cols, drop = FALSE])
  }

  rows_with_results <- cleaned_df[has_results, , drop = FALSE]
  expanded <- purrr::map(seq_len(nrow(rows_with_results)), function(idx) {
    base_row <- rows_with_results[idx, base_cols, drop = FALSE]
    clinician_rows <- rows_with_results$clinician_data[[idx]]
    clinician_rows <- as.data.frame(clinician_rows, stringsAsFactors = FALSE)

    base_expanded <- base_row[rep(1, nrow(clinician_rows)), , drop = FALSE]
    combined <- cbind(base_expanded, clinician_rows)
    as.data.frame(combined, stringsAsFactors = FALSE)
  })

  result <- dplyr::bind_rows(expanded)

  if (requireNamespace("beepr", quietly = TRUE)) beepr::beep(2)

  result
}
