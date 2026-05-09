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
#' @examplesIf interactive()
#' clinician_df <- mysterycall_get_clinician_data("clinicians.csv")
mysterycall_get_clinician_data <- function(input_data) {
  if (is.data.frame(input_data)) {
    clinician_df <- input_data
  } else if (is.character(input_data) && length(input_data) == 1) {
    if (!file.exists(input_data)) {
      stop(sprintf("CSV file not found: %s", input_data), call. = FALSE)
    }

    clinician_df <- readr::read_csv(
      input_data,
      col_types = readr::cols(.default = readr::col_guess(), npi = readr::col_character())
    )
  } else {
    stop(sprintf("`input_data` must be a data frame or a single CSV file path; received class: %s.", paste(class(input_data), collapse = ", ")), call. = FALSE)
  }

  if (!"npi" %in% names(clinician_df)) {
    stop(sprintf("`input_data` is missing required column `npi`. Available columns: %s", if (length(names(clinician_df))) paste(names(clinician_df), collapse = ", ") else "<none>"), call. = FALSE)
  }

  cleaned_df <- mysterycall_validate_npi(clinician_df)
  if (!nrow(cleaned_df)) {
    cleaned_df$npi_is_valid <- logical()
    return(cleaned_df)
  }

  get_clinician_data <- function(npi) {
    npi <- as.character(npi)
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

  if (isTRUE(interactive()) && requireNamespace("beepr", quietly = TRUE)) {
    beepr::beep(2)
  }

  result
}
