#' Search and Process NPI Numbers
#'
#' This function takes an input data frame containing first and last names, performs NPI search and processing,
#' and filters results based on specified taxonomies. It supports customization for enumeration type, search limit,
#' and filtering credentials.
#'
#' @param data A data frame with columns 'first' and 'last' containing the names to search.
#' @param enumeration_type The enumeration type for NPI search (e.g., "ind", "org", "all"). Default is "ind".
#' @param limit The maximum number of search results to request for each name pair. Default is 5.
#' @param country_code Filter for only the "US".
#' @param filter_credentials A character vector containing the credentials to filter the NPI results. Default is c("MD", "DO").
#' @param save_chunk_size The number of results to save per chunk. Default is 10.
#' @param dest_dir Destination directory to save chunked results. Default is NULL (no files written).
#' @return A data frame containing the processed NPI search results.
#'
#' @importFrom dplyr filter mutate select rename distinct arrange bind_rows tibble
#' @importFrom npi npi_search npi_flatten
#' @importFrom progress progress_bar
#' @importFrom purrr map2 keep
#' @importFrom readr write_csv
#' @family npi
#' @export
#' @examples
#' \dontrun{
#' df <- data.frame(first = "John", last = "Doe")
#' results <- search_and_process_npi(df)
#' }
search_and_process_npi <- function(data,
                                   enumeration_type = "ind",
                                   limit = 5L,
                                   country_code = "US",
                                   filter_credentials = c("MD", "DO"),
                                   save_chunk_size = 10,
                                   dest_dir = NULL) {
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }

  required_cols <- c("first", "last")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols)) {
    stop("Input data must contain columns: ", paste(missing_cols, collapse = ", "))
  }

  if (!nrow(data)) {
    return(dplyr::tibble())
  }

  first_names <- as.character(data$first)
  last_names <- as.character(data$last)

  vc <- c(
    "Allergy & Immunology", "Anesthesiology", "Dermatology", "Emergency Medicine",
    "Family Medicine", "Internal Medicine", "Obstetrics & Gynecology", "Ophthalmology",
    "Orthopaedic Surgery", "Pediatrics", "Psychiatry & Neurology", "Radiology", "Surgery", "Urology"
  )
  bc <- c("Pathology", "Pediatrics", "Physical Medicine & Rehabilitation", "Plastic Surgery", "Preventive Medicine")
  credential_filter <- NULL
  if (!is.null(filter_credentials)) {
    credential_filter <- tolower(gsub("[^A-Za-z0-9]", "", filter_credentials))
  }

  total_names <- length(first_names)
  pb <- NULL
  if (total_names > 0) {
    pb <- progress::progress_bar$new(total = total_names, clear = FALSE, show_after = 0)
  }

  save_results <- function(result, file_prefix, directory) {
    if (is.null(directory) || is.na(directory) || !nrow(result)) {
      return(invisible(NULL))
    }
    dir.create(directory, showWarnings = FALSE, recursive = TRUE)
    timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
    file_name <- file.path(directory, paste0(file_prefix, "_chunk_", timestamp, ".csv"))
    readr::write_csv(result, file_name)
  }

  search_npi <- function(first_name, last_name) {
    tryCatch({
      npi_obj <- npi::npi_search(
        first_name = first_name,
        last_name = last_name
      )

      if (is.null(npi_obj)) {
        return(NULL)
      }

      flattened <- npi::npi_flatten(npi_obj, cols = c("basic", "taxonomies"))
      if (is.null(flattened) || !nrow(flattened)) {
        return(NULL)
      }

      filtered <- dplyr::filter(flattened, taxonomies_desc %in% vc | taxonomies_desc %in% bc)
      if (!nrow(filtered)) {
        return(NULL)
      }

      if (!"basic_credential" %in% names(filtered)) {
        filtered$basic_credential <- NA_character_
      }
      if (!"basic_first_name" %in% names(filtered)) {
        filtered$basic_first_name <- NA_character_
      }
      if (!"basic_last_name" %in% names(filtered)) {
        filtered$basic_last_name <- NA_character_
      }
      if (!"basic_middle_name" %in% names(filtered)) {
        filtered$basic_middle_name <- NA_character_
      }

      filtered <- dplyr::mutate(
        filtered,
        credential = basic_credential,
        credential_clean = tolower(gsub("[^A-Za-z0-9]", "", credential))
      )

      if (!is.null(credential_filter)) {
        filtered <- dplyr::filter(filtered, is.na(credential_clean) | credential_clean %in% credential_filter)
      }

      filtered <- dplyr::rename(
        filtered,
        first_name = basic_first_name,
        last_name = basic_last_name,
        middle_name = basic_middle_name
      )

      filtered <- dplyr::mutate(filtered, search_term = paste(first_name, last_name))
      filtered <- dplyr::select(filtered, -credential_clean, -basic_credential)
      filtered <- dplyr::distinct(filtered, npi, taxonomies_desc, .keep_all = TRUE)
      filtered <- dplyr::arrange(filtered, last_name, first_name)
      filtered
    }, error = function(e) {
      message(sprintf("Error searching %s %s: %s", first_name, last_name, e$message))
      NULL
    })
  }

  out <- purrr::map2(first_names, last_names, function(first_name, last_name) {
    if (!is.null(pb)) {
      pb$tick()
    }
    result <- search_npi(first_name, last_name)

    should_save <- !is.null(result) && nrow(result) && is.numeric(save_chunk_size) && !is.na(save_chunk_size) && save_chunk_size > 0 && nrow(result) >= save_chunk_size
    if (should_save) {
      save_results(result, "results_of_search_and_process_npi", dest_dir)
    }
    result
  })

  npi_data <- purrr::keep(out, function(x) is.data.frame(x) && nrow(x) > 0)
  if (!length(npi_data)) {
    return(dplyr::tibble())
  }

  result <- dplyr::bind_rows(npi_data)

  if (requireNamespace("beepr", quietly = TRUE)) {
    if (requireNamespace("beepr", quietly = TRUE)) beepr::beep(2)
  }

  result
}
