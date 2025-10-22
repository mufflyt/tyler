#' Validate and Remove Invalid NPI Numbers
#'
#' This function reads a CSV file containing NPI numbers, validates their
#' format using the npi package, and removes rows with missing or invalid NPIs.
#'
#' @param input_data Either a dataframe containing NPI numbers or a path to a CSV file.
#'
#' @return A dataframe containing valid NPI numbers.
#'
#' @importFrom npi npi_is_valid
#' @importFrom readr read_csv cols col_character col_guess
#' @importFrom dplyr filter mutate
#' @export
validate_and_remove_invalid_npi <- function(input_data) {

  if (is.data.frame(input_data)) {
    npi_df <- input_data
  } else if (is.character(input_data) && length(input_data) == 1) {
    npi_df <- readr::read_csv(
      input_data,
      col_types = readr::cols(.default = readr::col_guess(), npi = readr::col_character())
    )
  } else {
    stop("Input must be a dataframe or a file path to a CSV.")
  }

  if (!"npi" %in% names(npi_df)) {
    stop("Input data must contain an 'npi' column.")
  }

  npi_df <- npi_df %>%
    dplyr::mutate(
      npi = as.character(npi),
      npi = gsub("[^0-9]", "", npi),
      npi = trimws(npi)
    ) %>%
    dplyr::filter(!is.na(npi) & npi != "")

  if (!nrow(npi_df)) {
    npi_df$npi_is_valid <- logical()
    return(npi_df[, unique(c("npi", "npi_is_valid", names(npi_df))), drop = FALSE])
  }

  valid_format <- nchar(npi_df$npi) == 10 & !grepl("\\D", npi_df$npi)
  npi_df$npi_is_valid <- FALSE

  if (any(valid_format)) {
    npi_df$npi_is_valid[valid_format] <- npi::npi_is_valid(npi_df$npi[valid_format])
  }

  npi_df <- npi_df %>%
    dplyr::filter(!is.na(npi_is_valid) & npi_is_valid)

  npi_df
}
