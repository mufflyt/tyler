#' Retrieve Clinician Data
#'
#' This function retrieves clinician data for each valid NPI in the input data frame.
#'
#' @param input_data Either a data frame containing NPI numbers or a path to a CSV file.
#'
#' @return A tibble with clinician data for the provided NPIs.
#' @importFrom readr read_csv cols col_character col_guess
#' @importFrom dplyr mutate bind_rows
#' @family npi
#' @export
#' @examples
#' \dontrun{
#' clinician_df <- retrieve_clinician_data("clinicians.csv")
#' }
retrieve_clinician_data <- function(input_data) {
  checkmate::assert(
    checkmate::check_data_frame(input_data),
    checkmate::check_string(input_data, min.chars = 1),
    .var.name = "input_data"
  )

  if (is.data.frame(input_data)) {
    checkmate::assert_true(nrow(input_data) > 0, .var.name = "input_data")
    checkmate::assert_names(names(input_data), type = "strict", .var.name = "names(input_data)")
    clinician_df <- input_data
  } else if (is.character(input_data) && length(input_data) == 1) {
    checkmate::assert_file_exists(input_data, access = "r", .var.name = "input_data")
    checkmate::assert_true(grepl("\.csv$", input_data, ignore.case = TRUE), .var.name = "input_data")

    clinician_df <- readr::read_csv(
      input_data,
      col_types = readr::cols(.default = readr::col_guess(), npi = readr::col_character())
    )
  } else {
    stop(sprintf("`input_data` must be a data frame or a single CSV file path; received class: %s.", paste(class(input_data), collapse = ", ")), call. = FALSE)
  }

  checkmate::assert_data_frame(clinician_df, min.rows = 1, .var.name = "clinician_df")

  if (!"npi" %in% names(clinician_df)) {
    stop(sprintf("`input_data` is missing required column `npi`. Available columns: %s", if (length(names(clinician_df))) paste(names(clinician_df), collapse = ", ") else "<none>"), call. = FALSE)
  }

  cleaned_df <- validate_and_remove_invalid_npi(clinician_df)
  checkmate::assert_data_frame(cleaned_df, .var.name = "cleaned_df")
  checkmate::assert_names(names(cleaned_df), must.include = "npi", .var.name = "names(cleaned_df)")
  if (!nrow(cleaned_df)) {
    cleaned_df$npi_is_valid <- logical()
    return(cleaned_df)
  }

  get_clinician_data <- function(npi) {
    checkmate::assert_atomic_vector(npi, len = 1, null.ok = FALSE, .var.name = "npi")
    npi <- as.character(npi)
    checkmate::assert_string(npi, na.ok = FALSE, .var.name = "npi")
    if (nchar(npi) != 10 || grepl("\\D", npi)) {
      return(NULL)
    }

    provider_ns <- tryCatch(asNamespace("provider"), error = function(e) NULL)
    if (is.null(provider_ns)) {
      message(sprintf(
        "NPI %s: package 'provider' is not installed. ",
        "Install from GitHub with remotes::install_github('andrewallenbruce/provider').",
        npi
      ))
      return(NULL)
    }
    clinicians_fn <- get0("clinicians", envir = provider_ns, mode = "function")
    if (is.null(clinicians_fn)) {
      return(NULL)
    }

    clinician_info <- tryCatch(
      clinicians_fn(npi = npi),
      error = function(e) {
        message(sprintf("provider::clinicians() failed for NPI %s: %s", npi, conditionMessage(e)))
        NULL
      }
    )
    if (is.null(clinician_info) || !nrow(clinician_info)) {
      return(NULL)
    }
    as.data.frame(clinician_info, stringsAsFactors = FALSE)
  }

  cleaned_df <- cleaned_df %>%
    dplyr::mutate(clinician_data = lapply(.data$npi, get_clinician_data))

  has_results <- vapply(cleaned_df$clinician_data, function(x) {
    !is.null(x) && nrow(x) > 0
  }, logical(1))

  base_cols <- setdiff(names(cleaned_df), "clinician_data")
  checkmate::assert_character(base_cols, min.len = 1, any.missing = FALSE, .var.name = "base_cols")

  if (!any(has_results)) {
    return(cleaned_df[FALSE, base_cols, drop = FALSE])
  }

  rows_with_results <- cleaned_df[has_results, , drop = FALSE]
  expanded <- lapply(seq_len(nrow(rows_with_results)), function(idx) {
    base_row <- rows_with_results[idx, base_cols, drop = FALSE]
    clinician_rows <- rows_with_results$clinician_data[[idx]]
    clinician_rows <- as.data.frame(clinician_rows, stringsAsFactors = FALSE)

    base_expanded <- base_row[rep(1, nrow(clinician_rows)), , drop = FALSE]
    combined <- cbind(base_expanded, clinician_rows)
    as.data.frame(combined, stringsAsFactors = FALSE)
  })

  result <- dplyr::bind_rows(expanded)
  checkmate::assert_data_frame(result, .var.name = "result")

  if (isTRUE(interactive()) && requireNamespace("beepr", quietly = TRUE)) {
    beepr::beep(2)
  }

  result
}
