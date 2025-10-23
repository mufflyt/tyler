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
#' @param accumulate_path Optional CSV path that accumulates every non-empty result.
#'   When provided, results are appended after each successful lookup.
#' @param resume Logical. If `TRUE` and `accumulate_path` already exists, names that
#'   have been processed previously (based on the `search_term` column) are skipped.
#' @param progress_log Optional file path used to log progress updates. The log file
#'   is appended to and created automatically if it does not exist.
#' @param notify Logical. If `TRUE`, play a notification sound when processing
#'   completes (requires the optional `beepr` package). Defaults to `TRUE`.
#' @param verbose Logical; if TRUE, prints status messages while running. Default is FALSE.
#' @return A data frame containing the processed NPI search results.
#'
#' @importFrom dplyr filter mutate select rename distinct arrange bind_rows tibble
#' @importFrom npi npi_search npi_flatten
#' @importFrom progress progress_bar
#' @importFrom purrr map2 keep
#' @importFrom readr write_csv read_csv
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
                                   dest_dir = NULL,
                                   accumulate_path = NULL,
                                   resume = FALSE,
                                   progress_log = NULL,
                                   notify = TRUE,
                                   verbose = FALSE) {
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }

  required_cols <- c("first", "last")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols)) {
    stop("Input data must contain columns: ", paste(missing_cols, collapse = ", "))
  }

  log_progress <- function(message, verbose = verbose) {
    if (!is.null(progress_log)) {
      dir.create(dirname(progress_log), showWarnings = FALSE, recursive = TRUE)
      cat(sprintf("[%s] %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), message),
          file = progress_log, append = TRUE)
    }
    if (isTRUE(verbose)) {
      message(message)
    }
  }

  read_existing_accumulation <- function(verbose = verbose) {
    if (is.null(accumulate_path) || !file.exists(accumulate_path)) {
      return(NULL)
    }
    tryCatch(
      readr::read_csv(accumulate_path, show_col_types = FALSE),
      error = function(e) {
        log_progress(sprintf("Unable to read accumulation file '%s': %s", accumulate_path, e$message), verbose = verbose)
        NULL
      }
    )
  }

  existing_accumulated <- NULL
  processed_terms <- character(0)
  if (isTRUE(resume)) {
    existing_accumulated <- read_existing_accumulation(verbose = verbose)
    if (!is.null(existing_accumulated) && "search_term" %in% names(existing_accumulated)) {
      processed_terms <- unique(existing_accumulated$search_term[!is.na(existing_accumulated$search_term)])
    }
  }

  if (!nrow(data)) {
    final_result <- if (!is.null(existing_accumulated)) existing_accumulated else dplyr::tibble()
    if (isTRUE(notify) && requireNamespace("beepr", quietly = TRUE)) {
      beepr::beep(2)
    }
    return(final_result)
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

  save_results <- function(result, file_prefix, directory, verbose = verbose) {
    if (is.null(directory) || is.na(directory) || !nrow(result)) {
      return(invisible(NULL))
    }
    dir.create(directory, showWarnings = FALSE, recursive = TRUE)
    timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
    file_name <- file.path(directory, paste0(file_prefix, "_chunk_", timestamp, ".csv"))
    readr::write_csv(result, file_name)
    log_progress(sprintf("Saved chunk '%s' with %d rows", basename(file_name), nrow(result)), verbose = verbose)
  }

  append_accumulated <- function(result, verbose = verbose) {
    if (is.null(accumulate_path) || is.na(accumulate_path) || !nrow(result)) {
      return(invisible(NULL))
    }
    dir.create(dirname(accumulate_path), showWarnings = FALSE, recursive = TRUE)
    append_mode <- file.exists(accumulate_path)
    tryCatch({
      readr::write_csv(result, accumulate_path, append = append_mode, col_names = !append_mode)
      log_progress(sprintf("Appended %d rows to '%s'", nrow(result), accumulate_path), verbose = verbose)
    }, error = function(e) {
      log_progress(sprintf("Unable to append to '%s': %s", accumulate_path, e$message), verbose = verbose)
    })
  }

  search_npi <- function(first_name, last_name, verbose = verbose) {
    tryCatch({
      npi_obj <- npi::npi_search(
        first_name = first_name,
        last_name = last_name,
        enumeration_type = enumeration_type,
        limit = limit,
        country_code = country_code
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
      if (isTRUE(verbose)) {
        message(sprintf("Error searching %s %s: %s", first_name, last_name, e$message))
      }
      NULL
    })
  }

  out <- purrr::map2(first_names, last_names, function(first_name, last_name) {
    if (!is.null(pb)) {
      pb$tick()
    }
    search_term <- trimws(paste(first_name, last_name))
    if (isTRUE(resume) && length(processed_terms) && search_term %in% processed_terms) {
      log_progress(sprintf("Skipping %s (already present in accumulation)", search_term))
      return(NULL)
    }

    result <- search_npi(first_name, last_name, verbose = verbose)

    if (is.null(result) || !nrow(result)) {
      log_progress(sprintf("No results for %s", search_term))
      return(NULL)
    }

    should_save <- is.numeric(save_chunk_size) && !is.na(save_chunk_size) && save_chunk_size > 0 && nrow(result) >= save_chunk_size
    if (should_save) {
      save_results(result, "results_of_search_and_process_npi", dest_dir, verbose = verbose)
    }

    append_accumulated(result, verbose = verbose)
    log_progress(sprintf("Retrieved %d rows for %s", nrow(result), search_term))
    
    result
  })

  npi_data <- purrr::keep(out, function(x) is.data.frame(x) && nrow(x) > 0)
  if (!length(npi_data)) {
    final_result <- if (!is.null(existing_accumulated)) existing_accumulated else dplyr::tibble()
  } else {
    result <- dplyr::bind_rows(npi_data)
    if (!is.null(existing_accumulated)) {
      final_result <- dplyr::bind_rows(existing_accumulated, result)
      final_result <- dplyr::distinct(final_result)
    } else {
      final_result <- result
    }
  }

  log_progress(sprintf("Completed processing. Returning %d rows.", nrow(final_result)))

  if (isTRUE(notify) && requireNamespace("beepr", quietly = TRUE)) {
    beepr::beep(2)
  }

  final_result
}
