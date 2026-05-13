#' Validate and Remove Invalid NPI Numbers
#'
#' This function reads a CSV file containing NPI numbers, validates their
#' format using the npi package, and removes rows with missing or invalid NPIs.
#'
#' @param input_data Either a data frame containing NPI numbers or a path to a CSV file.
#'
#' @return A data frame containing valid NPI numbers.
#'
#' @importFrom npi npi_is_valid
#' @importFrom readr read_csv cols col_character col_guess
#' @importFrom dplyr filter mutate
#' @export
validate_and_remove_invalid_npi <- function(input_data) {

  checkmate::assert(
    checkmate::check_data_frame(input_data),
    checkmate::check_string(input_data, min.chars = 1, len = 1),
    .var.name = "input_data"
  )

  if (is.data.frame(input_data)) {
    npi_df <- input_data
  } else if (is.character(input_data) && length(input_data) == 1) {
    checkmate::assert_true(!is.na(input_data), .var.name = "input_data")
    checkmate::assert_true(nzchar(trimws(input_data)), .var.name = "input_data")
    checkmate::assert_true(grepl("\\.csv$", input_data, ignore.case = TRUE), .var.name = "input_data")

    if (!file.exists(input_data)) {
      stop(sprintf("CSV file not found: %s", input_data), call. = FALSE)
    }

    npi_df <- readr::read_csv(
      input_data,
      col_types = readr::cols(.default = readr::col_guess(), npi = readr::col_character())
    )
    checkmate::assert_data_frame(npi_df, .var.name = "npi_df")
  } else {
    stop(sprintf("`input_data` must be a data frame or a single CSV file path; received class: %s.", paste(class(input_data), collapse = ", ")), call. = FALSE)
  }

  checkmate::assert_data_frame(npi_df, .var.name = "npi_df")
  checkmate::assert_names(names(npi_df), type = "unique", .var.name = "names(npi_df)")

  if (!"npi" %in% names(npi_df)) {
    stop(sprintf("`input_data` is missing required column `npi`. Available columns: %s", if (length(names(npi_df))) paste(names(npi_df), collapse = ", ") else "<none>"), call. = FALSE)
  }

  checkmate::assert_atomic_vector(npi_df$npi, .var.name = "npi_df$npi")

  npi_df <- npi_df %>%
    dplyr::mutate(
      npi = as.character(npi),
      npi = gsub("[^0-9]", "", npi),
      npi = trimws(npi)
    ) %>%
    dplyr::filter(!is.na(npi) & npi != "")

  checkmate::assert_data_frame(npi_df, .var.name = "npi_df")

  total_candidates <- nrow(npi_df)
  checkmate::assert_count(total_candidates, positive = FALSE, .var.name = "total_candidates")

  if (!total_candidates) {
    npi_df$npi_is_valid <- logical()
    message("No NPI values remained after removing blanks; returning empty result.")
    return(npi_df[, unique(c("npi", "npi_is_valid", names(npi_df))), drop = FALSE])
  }

  checkmate::assert_atomic_vector(npi_df$npi, any.missing = FALSE, .var.name = "npi_df$npi")

  valid_format <- nchar(npi_df$npi) == 10 & !grepl("\\D", npi_df$npi)
  checkmate::assert_logical(valid_format, any.missing = FALSE, len = nrow(npi_df), .var.name = "valid_format")
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

  checkmate::assert_true(all(npi_df$npi_is_valid), .var.name = "npi_df$npi_is_valid")

  message(sprintf(
    "Validated %d candidate NPI(s); %d passed checksum and formatting rules.",
    total_candidates,
    nrow(npi_df)
  ))

  npi_df
}
